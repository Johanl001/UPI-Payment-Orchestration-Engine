{- |
Module      : API.Handlers
Description : Servant handler implementations.

Each handler:
  1. Validates and converts the API request into a domain type.
  2. Calls the orchestrator (or repository directly for read paths).
  3. Converts the domain result back into an API response type.

Errors are returned as Servant's @throwError@ with an appropriate HTTP status.
-}
module API.Handlers
  ( AppEnv (..)
  , mkAppEnv
  , initiateHandler
  , statusHandler
  , callbackHandler
  , auditHandler
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, pack)
import Data.Time (getCurrentTime)
import Data.UUID (fromText, toText)
import Numeric.Natural (Natural)
import Servant

import API.Types
import Domain.Gateway (GatewayId (..))
import Domain.Transaction
import Orchestrator.Flow
import Persistence.Repository (DbPool, mkPool, getTransaction, getEventHistory, updateTransactionStatus)
import Persistence.Schema (TransactionRecord (..), EventRecord (..))

-- ---------------------------------------------------------------------------
-- Application environment
-- ---------------------------------------------------------------------------

-- | Everything a handler needs, passed explicitly (no global state).
data AppEnv = AppEnv
  { appOrchEnv :: !OrchestratorEnv
  , appDbPool  :: !DbPool
  }

mkAppEnv :: Text -> IO AppEnv
mkAppEnv dbPath = do
  orch <- mkOrchestratorEnv
  pool <- mkPool dbPath
  pure $ AppEnv orch pool

-- ---------------------------------------------------------------------------
-- POST /transactions — initiate a payment
-- ---------------------------------------------------------------------------

initiateHandler :: AppEnv -> InitiateRequest -> Handler InitiateResponse
initiateHandler AppEnv{..} req = do
  -- Basic validation
  when (initiateAmountPaise req <= 0) $
    throwError err400 { errBody = "Amount must be positive" }
  when (null (initiatePayerVpa req)) $
    throwError err400 { errBody = "payerVpa is required" }
  when (null (initiatePayeeVpa req)) $
    throwError err400 { errBody = "payeeVpa is required" }

  -- Build domain request
  let domainReq = TransactionRequest
        { treqPayerVPA  = VPA (initiatePayerVpa req)
        , treqPayeeVPA  = VPA (initiatePayeeVpa req)
        , treqAmount    = Amount (fromIntegral (initiateAmountPaise req) :: Natural)
        , treqIdemKey   = IdempotencyKey (initiateIdemKey req)
        }

  -- Run orchestration
  result <- liftIO $ runOrchestrator appOrchEnv (runTransaction domainReq)

  case result of
    Left (OrchestratorDBError msg) ->
      throwError err500 { errBody = "DB error: " <> show msg }
    Left (OrchestratorInternalError msg) ->
      throwError err500 { errBody = "Internal error: " <> show msg }

    Right (TxnResultSuccess tid gwRef gid attempts) ->
      pure InitiateResponse
        { initiateRespTxnId     = toText (unTransactionId tid)
        , initiateRespStatus    = "Success"
        , initiateRespGatewayRef = Just gwRef
        , initiateRespMessage   = "Payment processed successfully via " <> unGatewayId gid
        }

    Right (TxnResultFailed tid reason attempts) ->
      pure InitiateResponse
        { initiateRespTxnId     = toText (unTransactionId tid)
        , initiateRespStatus    = "Failed"
        , initiateRespGatewayRef = Nothing
        , initiateRespMessage   = "Payment failed: " <> pack (show reason)
        }

    Right (TxnResultIdempotentHit tid status) ->
      pure InitiateResponse
        { initiateRespTxnId     = toText (unTransactionId tid)
        , initiateRespStatus    = status
        , initiateRespGatewayRef = Nothing
        , initiateRespMessage   = "Idempotent response — already processed"
        }

-- ---------------------------------------------------------------------------
-- GET /transactions/:id — check current status
-- ---------------------------------------------------------------------------

statusHandler :: AppEnv -> Text -> Handler TransactionStatusResponse
statusHandler AppEnv{..} tidText = do
  mUuid <- pure (fromText tidText)
  case mUuid of
    Nothing -> throwError err400 { errBody = "Invalid transaction ID format" }
    Just uuid -> do
      let tid = TransactionId uuid
      mrec <- liftIO $ getTransaction appDbPool tid
      case mrec of
        Nothing -> throwError err404 { errBody = "Transaction not found" }
        Just rec -> pure $ TransactionStatusResponse
          { statusRespTxnId      = transactionRecordTxnId rec
          , statusRespStatus     = transactionRecordStatus rec
          , statusRespGatewayId  = transactionRecordGatewayId rec
          , statusRespGatewayRef = transactionRecordGatewayRef rec
          , statusRespFailure    = transactionRecordFailureReason rec
          , statusRespRetryCount = transactionRecordRetryCount rec
          , statusRespCreatedAt  = transactionRecordCreatedAt rec
          , statusRespUpdatedAt  = transactionRecordUpdatedAt rec
          }

-- ---------------------------------------------------------------------------
-- POST /webhook/gateway-callback — simulate async gateway notification
-- ---------------------------------------------------------------------------

{- | In real UPI flows, gateways send async callbacks when a transaction
     completes asynchronously. This endpoint simulates receiving such a callback
     and updating the transaction state accordingly.
-}
callbackHandler :: AppEnv -> GatewayCallbackPayload -> Handler GatewayCallbackResponse
callbackHandler AppEnv{..} payload = do
  mUuid <- pure (fromText (callbackTxnId payload))
  case mUuid of
    Nothing -> throwError err400 { errBody = "Invalid txnId in callback" }
    Just uuid -> do
      let tid = TransactionId uuid
          gid = Just (GatewayId (callbackGatewayId payload))
          (newStatus, mRef, mFail) = case callbackStatus payload of
            "SUCCESS" -> ("Success", callbackGatewayRef payload, Nothing)
            "FAILED"  -> ("Failed",  Nothing, callbackFailCode payload)
            _         -> ("Pending", Nothing, Nothing)
      liftIO $ updateTransactionStatus appDbPool tid newStatus gid mRef mFail 0
      pure $ GatewayCallbackResponse { callbackRespAck = "ACK" }

-- ---------------------------------------------------------------------------
-- GET /transactions/:id/audit — full event history
-- ---------------------------------------------------------------------------

auditHandler :: AppEnv -> Text -> Handler AuditHistoryResponse
auditHandler AppEnv{..} tidText = do
  mUuid <- pure (fromText tidText)
  case mUuid of
    Nothing -> throwError err400 { errBody = "Invalid transaction ID format" }
    Just uuid -> do
      let tid = TransactionId uuid
      events <- liftIO $ getEventHistory appDbPool tid
      pure $ AuditHistoryResponse
        { auditTxnId  = tidText
        , auditEvents = map toAuditEvent events
        }

toAuditEvent :: EventRecord -> AuditEventResponse
toAuditEvent rec = AuditEventResponse
  { auditEventId   = eventRecordEventId rec
  , auditSeqNum    = eventRecordSeqNum rec
  , auditEventType = eventRecordEventType rec
  , auditPayload   = eventRecordEventPayload rec
  , auditTimestamp = eventRecordCreatedAt rec
  }
