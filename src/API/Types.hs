{- |
Module      : API.Types
Description : JSON request/response types for the REST API.

These are intentionally separate from domain types:
  * Domain types carry type-level state machine information (GADTs) that can't
    be serialised directly.
  * API types are flat records with Aeson instances — stable external contract.
  * The conversion functions (toAPI*, fromAPI*) form an anti-corruption layer.
-}
module API.Types
  ( -- * POST /transactions
    InitiateRequest (..)
  , InitiateResponse (..)

    -- * GET /transactions/:id
  , TransactionStatusResponse (..)

    -- * POST /webhook/gateway-callback
  , GatewayCallbackPayload (..)
  , GatewayCallbackResponse (..)

    -- * GET /transactions/:id/audit
  , AuditEventResponse (..)
  , AuditHistoryResponse (..)

    -- * Error
  , ApiError (..)
  ) where

import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- ---------------------------------------------------------------------------
-- POST /transactions
-- ---------------------------------------------------------------------------

data InitiateRequest = InitiateRequest
  { initiatePayerVpa    :: !Text
  , initiatePayeeVpa    :: !Text
  , initiateAmountPaise :: !Int   -- ^ Amount in smallest unit (paise)
  , initiateIdemKey     :: !Text  -- ^ Caller-supplied idempotency key
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data InitiateResponse = InitiateResponse
  { initiateRespTxnId    :: !Text
  , initiateRespStatus   :: !Text
  , initiateRespGatewayRef :: !(Maybe Text)
  , initiateRespMessage  :: !Text
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- ---------------------------------------------------------------------------
-- GET /transactions/:id
-- ---------------------------------------------------------------------------

data TransactionStatusResponse = TransactionStatusResponse
  { statusRespTxnId       :: !Text
  , statusRespStatus      :: !Text
  , statusRespGatewayId   :: !(Maybe Text)
  , statusRespGatewayRef  :: !(Maybe Text)
  , statusRespFailure     :: !(Maybe Text)
  , statusRespRetryCount  :: !Int
  , statusRespCreatedAt   :: !UTCTime
  , statusRespUpdatedAt   :: !UTCTime
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- ---------------------------------------------------------------------------
-- POST /webhook/gateway-callback
-- ---------------------------------------------------------------------------

data GatewayCallbackPayload = GatewayCallbackPayload
  { callbackTxnId      :: !Text
  , callbackGatewayId  :: !Text
  , callbackGatewayRef :: !(Maybe Text)
  , callbackStatus     :: !Text   -- ^ "SUCCESS" | "FAILED" | "PENDING"
  , callbackFailCode   :: !(Maybe Text)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data GatewayCallbackResponse = GatewayCallbackResponse
  { callbackRespAck :: !Text }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- ---------------------------------------------------------------------------
-- GET /transactions/:id/audit
-- ---------------------------------------------------------------------------

data AuditEventResponse = AuditEventResponse
  { auditEventId    :: !Text
  , auditSeqNum     :: !Int
  , auditEventType  :: !Text
  , auditPayload    :: !Text  -- ^ Raw JSON payload
  , auditTimestamp  :: !UTCTime
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

data AuditHistoryResponse = AuditHistoryResponse
  { auditTxnId  :: !Text
  , auditEvents :: ![AuditEventResponse]
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- ---------------------------------------------------------------------------
-- Generic API error
-- ---------------------------------------------------------------------------

data ApiError = ApiError
  { apiErrorCode    :: !Text
  , apiErrorMessage :: !Text
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)
