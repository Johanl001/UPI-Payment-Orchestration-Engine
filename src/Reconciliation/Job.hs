{- |
Module      : Reconciliation.Job
Description : Background job that reconciles Pending transactions.

Why reconciliation?
  In UPI, a payment can be in flight when the gateway doesn't respond in time
  (timeout). The orchestrator marks it TimedOut/Pending, but the bank may have
  debited the payer. Without reconciliation, the transaction would remain stuck
  forever. This job polls Pending transactions and asks the gateway for the
  real outcome.

  This mirrors Juspay's actual reconciliation pipeline — one of their core
  products is reconciliation-as-a-service for payment aggregators.

The job runs as a background 'async' thread, waking up every @intervalSeconds@.
-}
module Reconciliation.Job
  ( ReconciliationConfig (..)
  , defaultReconciliationConfig
  , startReconciliationJob
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (async, Async)
import Control.Monad.Except (runExceptT)
import Data.Text (pack)
import Data.Time (getCurrentTime, NominalDiffTime)
import Data.UUID (fromText)
import System.IO (hPutStrLn, stderr)

import Domain.Events (TransactionEvent (..))
import Domain.Gateway
import Domain.Transaction
import Gateway.Class
import Gateway.MockGatewayA (mkMockGatewayA)
import Gateway.MockGatewayB (mkMockGatewayB)
import Gateway.MockGatewayC (mkMockGatewayC)
import Persistence.Repository (DbPool, getPendingOlderThan, updateTransactionStatus, appendEvent)
import Persistence.Schema (TransactionRecord (..))
import Persistence.SchemaDefs (transactionRecordTxnId, transactionRecordGatewayId, transactionRecordIdempotencyKey, transactionRecordRetryCount)
import Orchestrator.Flow (OrchestratorEnv (..))
import Domain.Events (EventId (..), mkEventEnvelope)
import Data.UUID.V4 (nextRandom)
import Control.Concurrent.STM (atomically, modifyTVar', readTVar)
import qualified Data.Map.Strict as Map

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

data ReconciliationConfig = ReconciliationConfig
  { reconcileIntervalSecs  :: !Int        -- ^ How often the job wakes up (e.g. 30)
  , reconcileAgeSecs       :: !NominalDiffTime  -- ^ How old a Pending txn must be (e.g. 60)
  } deriving stock (Show, Eq)

defaultReconciliationConfig :: ReconciliationConfig
defaultReconciliationConfig = ReconciliationConfig
  { reconcileIntervalSecs = 30
  , reconcileAgeSecs      = 60
  }

-- ---------------------------------------------------------------------------
-- Background job launcher
-- ---------------------------------------------------------------------------

-- | Spawn the reconciliation job as a background async thread.
--   Returns the Async handle so the caller can cancel it on shutdown.
startReconciliationJob
  :: ReconciliationConfig
  -> DbPool
  -> OrchestratorEnv
  -> IO (Async ())
startReconciliationJob cfg pool env =
  async $ reconcileLoop cfg pool env

reconcileLoop :: ReconciliationConfig -> DbPool -> OrchestratorEnv -> IO ()
reconcileLoop cfg pool env = do
  threadDelay (reconcileIntervalSecs cfg * 1_000_000)
  runReconciliationCycle cfg pool env
  reconcileLoop cfg pool env

-- ---------------------------------------------------------------------------
-- One reconciliation cycle
-- ---------------------------------------------------------------------------

runReconciliationCycle :: ReconciliationConfig -> DbPool -> OrchestratorEnv -> IO ()
runReconciliationCycle ReconciliationConfig{..} pool env = do
  pending <- getPendingOlderThan pool reconcileAgeSecs
  logInfo $ "Reconciliation: found " <> show (length pending) <> " pending transactions"
  mapM_ (reconcileTxn pool env) pending

reconcileTxn :: DbPool -> OrchestratorEnv -> TransactionRecord -> IO ()
reconcileTxn pool env rec = do
  let tidText = transactionRecordTxnId rec
      mgidText = transactionRecordGatewayId rec
  case (fromText tidText, mgidText) of
    (Nothing, _)      -> logInfo $ "Reconcile: invalid UUID " <> show tidText
    (_, Nothing)      -> logInfo $ "Reconcile: no gatewayId for " <> show tidText
    (Just uuid, Just gidText) -> do
      let tid = TransactionId uuid
          gid = GatewayId gidText
          ikey = IdempotencyKey (transactionRecordIdempotencyKey rec)
          statusReq = GatewayStatusRequest
            { statusReqTxnId   = tid
            , statusReqIdemKey = ikey
            }
      response <- checkGatewayStatus gid statusReq
      now <- getCurrentTime
      case response of
        Left err -> do
          logInfo $ "Reconcile: gateway check failed for " <> show tidText <> ": " <> show err

        Right GatewayStatusResponse{..} -> do
          let retries = transactionRecordRetryCount rec
          case statusRespStatus of
            GwSuccess gwRef -> do
              updateTransactionStatus pool tid "Success" (Just gid) (Just gwRef) Nothing retries
              eid <- EventId <$> nextRandom
              seqN <- bumpSeq env tid
              let evt = mkEventEnvelope eid tid seqN now
                          (Domain.Events.TxnReconciled (ConfirmedSuccess gid gwRef))
              appendEvent pool evt
              logInfo $ "Reconcile: SUCCESS " <> show tidText

            GwPending ->
              logInfo $ "Reconcile: still Pending " <> show tidText <> " — will retry next cycle"

            GwFailed (Terminal reason) -> do
              let failText = pack (show reason)
              updateTransactionStatus pool tid "Failed" (Just gid) Nothing (Just failText) retries
              eid  <- EventId <$> nextRandom
              seqN <- bumpSeq env tid
              let evt = mkEventEnvelope eid tid seqN now
                          (Domain.Events.TxnReconciled (ConfirmedFailed gid reason))
              appendEvent pool evt
              logInfo $ "Reconcile: FAILED " <> show tidText

            GwFailed (Retryable _) ->
              logInfo $ "Reconcile: transient failure for " <> show tidText <> " — will retry"

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Dispatch to the correct mock gateway for a status check.
checkGatewayStatus
  :: GatewayId
  -> GatewayStatusRequest
  -> IO (Either GatewayError GatewayStatusResponse)
checkGatewayStatus gid req = case unGatewayId gid of
  "gateway-a" -> runExceptT $ checkStatus mkMockGatewayA req
  "gateway-b" -> runExceptT $ checkStatus mkMockGatewayB req
  "gateway-c" -> runExceptT $ checkStatus mkMockGatewayC req
  _           -> pure $ Left (GwServerError 404 "Unknown gateway")

-- | Increment the per-transaction event sequence counter.
bumpSeq :: OrchestratorEnv -> TransactionId -> IO Int
bumpSeq OrchestratorEnv{..} tid =
  atomically $ do
    m <- readTVar envSeqCounters
    let n = Map.findWithDefault 0 tid m + 1
    modifyTVar' envSeqCounters (Map.insert tid n)
    pure n

logInfo :: String -> IO ()
logInfo msg = hPutStrLn stderr $ "[Reconciliation] " <> msg
