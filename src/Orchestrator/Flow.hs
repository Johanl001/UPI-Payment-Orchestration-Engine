{- |
Module      : Orchestrator.Flow
Description : Main orchestration logic — wires together routing, retry, and persistence.

The OrchestratorM monad stack:
  @type OrchestratorM a = ReaderT OrchestratorEnv (ExceptT OrchestratorError IO) a@

Why this stack?
  * ReaderT: carries the environment (router, idempotency store, DB pool, logger)
    without threading it manually through every function.
  * ExceptT: models orchestration-level errors that should abort the whole flow
    (e.g., DB connection lost) separately from gateway-level failures handled by retry.
  * IO at the base: we need real concurrency (STM, threadDelay) and gateway calls.

This mirrors EulerHS's Flow monad, which is also a ReaderT over IO with structured
error handling — the key difference being EulerHS uses a Free monad for
testability; we use MTL-style because it's simpler to explain in an interview.
-}
module Orchestrator.Flow
  ( OrchestratorEnv (..)
  , OrchestratorError (..)
  , OrchestratorM
  , TransactionRequest (..)
  , TransactionResult (..)
  , mkOrchestratorEnv
  , runTransaction
  , runOrchestrator
  ) where

import Control.Concurrent.STM
import Control.Monad.Except
import Control.Monad.Reader
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Data.UUID.V4 (nextRandom)

import Domain.Gateway
import Domain.Transaction
import Domain.Events
import Gateway.Class
import Gateway.MockGatewayA (MockGatewayA (..), mkMockGatewayA)
import Gateway.MockGatewayB (MockGatewayB (..), mkMockGatewayB)
import Gateway.MockGatewayC (MockGatewayC (..), mkMockGatewayC)
import Orchestrator.Router
import Orchestrator.Retry

-- ---------------------------------------------------------------------------
-- Orchestrator environment
-- ---------------------------------------------------------------------------

{- | Everything the orchestrator needs, injected via ReaderT.
     This makes the orchestrator testable: swap out any component for a mock.
-}
data OrchestratorEnv = OrchestratorEnv
  { envRouter        :: !RouterEnv
  , envIdemStore     :: !IdempotencyStore
  , envRetryPolicy   :: !RetryPolicy
  , envEventLog      :: !(TVar [EventEnvelope])   -- ^ In-memory audit log (persisted via Repository in Step 4)
  , envSeqCounters   :: !(TVar (Map TransactionId Int))  -- ^ Per-txn event sequence numbers
  }

mkOrchestratorEnv :: IO OrchestratorEnv
mkOrchestratorEnv = do
  router   <- mkRouterEnv [GatewayId "gateway-a", GatewayId "gateway-b", GatewayId "gateway-c"]
  idem     <- newIdempotencyStore
  evtLog   <- newTVarIO []
  seqCnt   <- newTVarIO Map.empty
  pure $ OrchestratorEnv router idem defaultRetryPolicy evtLog seqCnt

-- ---------------------------------------------------------------------------
-- Orchestrator monad
-- ---------------------------------------------------------------------------

data OrchestratorError
  = OrchestratorDBError Text
  | OrchestratorInternalError Text
  deriving stock (Show, Eq)

type OrchestratorM a = ReaderT OrchestratorEnv (ExceptT OrchestratorError IO) a

-- | Run an OrchestratorM action, unwrapping to IO.
runOrchestrator :: OrchestratorEnv -> OrchestratorM a -> IO (Either OrchestratorError a)
runOrchestrator env m = runExceptT (runReaderT m env)

-- ---------------------------------------------------------------------------
-- Request/response types (thin layer over domain types)
-- ---------------------------------------------------------------------------

data TransactionRequest = TransactionRequest
  { treqPayerVPA    :: !VPA
  , treqPayeeVPA    :: !VPA
  , treqAmount      :: !Amount
  , treqIdemKey     :: !IdempotencyKey
  } deriving stock (Show, Eq)

data TransactionResult
  = TxnResultSuccess
      { tresId         :: !TransactionId
      , tresGatewayRef :: !Text
      , tresGatewayId  :: !GatewayId
      , tresAttempts   :: !Int
      }
  | TxnResultFailed
      { tresId         :: !TransactionId
      , tresReason     :: !TerminalFailure
      , tresAttempts   :: !Int
      }
  | TxnResultIdempotentHit
      { tresId         :: !TransactionId
      , tresStatus     :: !Text
      }
  deriving stock (Show, Eq)

-- ---------------------------------------------------------------------------
-- Main orchestration entry point
-- ---------------------------------------------------------------------------

{- | Run a UPI transaction through the full orchestration pipeline:
     1. Check idempotency key — short-circuit if already processed.
     2. Create a Transaction 'Initiated with a fresh UUID.
     3. Append TxnCreated event to audit log.
     4. Ask the router for the ordered gateway list.
     5. Attempt each gateway in order, with retry on transient failures.
     6. Record the final outcome and update idempotency store.
     7. Return a TransactionResult to the API layer.
-}
runTransaction :: TransactionRequest -> OrchestratorM TransactionResult
runTransaction req = do
  env <- ask

  -- Step 1: Idempotency check
  mEntry <- liftIO $ checkIdempotency (envIdemStore env) (treqIdemKey req)
  case mEntry of
    Just (IdemSuccess tid gid) ->
      pure $ TxnResultIdempotentHit tid "Success"
    Just (IdemFailed tid _) ->
      pure $ TxnResultIdempotentHit tid "Failed"
    Just (IdemPending tid) ->
      pure $ TxnResultIdempotentHit tid "Pending"
    Nothing -> do
      -- Step 2: Create transaction
      tid  <- liftIO $ TransactionId <$> nextRandom
      now  <- liftIO getCurrentTime
      let txnInit = initiateTransaction
                      tid
                      (treqIdemKey req)
                      (treqPayerVPA req)
                      (treqPayeeVPA req)
                      (treqAmount req)
                      now

      -- Step 3: Append TxnCreated event
      appendEvent env tid $ TxnCreated
        { evtPayerVPA = unVPA (treqPayerVPA req)
        , evtPayeeVPA = unVPA (treqPayeeVPA req)
        , evtAmount   = fromIntegral (unAmount (treqAmount req))
        , evtIdemKey  = unIdempotencyKey (treqIdemKey req)
        }

      -- Mark as in-flight in idempotency store
      liftIO $ recordIdempotency (envIdemStore env) (treqIdemKey req) (IdemPending tid)

      -- Step 4: Get ordered gateways
      gateways <- liftIO $ selectGateway (envRouter env)

      -- Step 5: Attempt with retry/fallback
      result <- liftIO $ attemptWithFallback env tid txnInit gateways (envRetryPolicy env)

      -- Step 6 & 7: Record outcome
      case result of
        TxnResultSuccess{..} -> do
          liftIO $ recordIdempotency (envIdemStore env) (treqIdemKey req)
                     (IdemSuccess tresId tresGatewayId)
          pure result
        TxnResultFailed{..} -> do
          liftIO $ recordIdempotency (envIdemStore env) (treqIdemKey req)
                     (IdemFailed tresId tresReason)
          pure result
        other -> pure other

-- ---------------------------------------------------------------------------
-- Gateway fallback loop
-- ---------------------------------------------------------------------------

{- | Try each gateway in the ranked order. On a retryable failure, move to the
     next gateway. On a terminal failure, stop immediately. If all gateways
     are exhausted, return the last retryable failure as a terminal outcome
     (at-max-retries = transaction failed).
-}
attemptWithFallback
  :: OrchestratorEnv
  -> TransactionId
  -> Transaction 'Initiated
  -> [GatewayId]
  -> RetryPolicy
  -> IO TransactionResult
attemptWithFallback env tid txnInit [] _ =
  -- No gateways left — this is a system-level failure
  pure $ TxnResultFailed tid (InvalidVPA "No gateways available") 0
attemptWithFallback env tid txnInit (gid:rest) policy = do
  now <- getCurrentTime

  -- Log: sending to gateway
  appendEventIO env tid $ TxnSentToGateway gid 1

  -- Mark pending in state machine (type-checked transition)
  let txnPending = markPending txnInit gid now

  -- Attempt the gateway call
  outcome <- withRetry policy initialRetryState (callGateway env gid txnInit)

  case outcome of
    RetrySuccess resp state -> do
      successTime <- getCurrentTime
      appendEventIO env tid $ TxnSucceededAt gid (respGatewayRef resp)
      recordGatewayResult (envRouter env) gid GwResultSuccess
      pure $ TxnResultSuccess
        { tresId         = tid
        , tresGatewayRef = respGatewayRef resp
        , tresGatewayId  = gid
        , tresAttempts   = retryAttemptNum state
        }

    RetryTerminal termFailure -> do
      failTime <- getCurrentTime
      appendEventIO env tid $ TxnTerminallyFailed gid termFailure
      recordGatewayResult (envRouter env) gid GwResultFailure
      pure $ TxnResultFailed
        { tresId       = tid
        , tresReason   = termFailure
        , tresAttempts = 1
        }

    RetryExhausted retryFailure state -> do
      -- This gateway exhausted retries — try the next one
      recordGatewayResult (envRouter env) gid GwResultFailure
      if null rest
        then pure $ TxnResultFailed tid InsufficientFunds 3  -- fallback terminal
        else attemptWithFallback env tid txnInit rest policy

-- ---------------------------------------------------------------------------
-- Single gateway call (lifted into IO)
-- ---------------------------------------------------------------------------

-- | Invoke the correct mock gateway based on GatewayId.
--   In production this would be a typeclass dispatch via a registry.
callGateway
  :: OrchestratorEnv
  -> GatewayId
  -> Transaction 'Initiated
  -> IO (Either GatewayFailure GatewayPaymentResponse)
callGateway env gid txnInit = do
  now <- getCurrentTime
  let core = txnCore txnInit
      gwReq = GatewayPaymentRequest
        { reqTxnId    = txnCoreId core
        , reqIdemKey  = txnCoreIdemKey core
        , reqPayerVPA = txnCorePayerVPA core
        , reqPayeeVPA = txnCorePayeeVPA core
        , reqAmount   = txnCoreAmount core
        , reqTimestamp = now
        }
  result <- case unGatewayId gid of
    "gateway-a" -> runExceptT $ initiatePayment mkMockGatewayA gwReq
    "gateway-b" -> runExceptT $ initiatePayment mkMockGatewayB gwReq
    "gateway-c" -> runExceptT $ initiatePayment mkMockGatewayC gwReq
    _           -> pure $ Left (GwServerError 404 "Unknown gateway")

  -- Translate GatewayError → GatewayFailure (Retryable/Terminal split)
  pure $ case result of
    Right resp -> Right resp
    Left (GwTimeout _)          -> Left (Retryable GatewayTimeout)
    Left (GwServerError 422 body)
      | "INSUFFICIENT_FUNDS" `T.isInfixOf` body
                                -> Left (Terminal InsufficientFunds)
    Left (GwServerError _ _)    -> Left (Retryable (GatewayServerError "5xx"))
    Left (GwNetworkFailure msg) -> Left (Retryable (NetworkError msg))
    Left GwRateLimited          -> Left (Retryable ThrottledByGateway)
    Left (GwUnexpectedResponse _) -> Left (Retryable (GatewayServerError "parse-error"))

-- ---------------------------------------------------------------------------
-- Event logging helpers
-- ---------------------------------------------------------------------------

appendEvent :: OrchestratorEnv -> TransactionId -> TransactionEvent -> OrchestratorM ()
appendEvent env tid evt = liftIO $ appendEventIO env tid evt

appendEventIO :: OrchestratorEnv -> TransactionId -> TransactionEvent -> IO ()
appendEventIO OrchestratorEnv{..} tid evt = do
  eid  <- EventId <$> nextRandom
  now  <- getCurrentTime
  seqN <- atomically $ do
    m   <- readTVar envSeqCounters
    let n = Map.findWithDefault 0 tid m + 1
    writeTVar envSeqCounters (Map.insert tid n m)
    pure n
  let envelope = mkEventEnvelope eid tid seqN now evt
  atomically $ modifyTVar' envEventLog (envelope :)
