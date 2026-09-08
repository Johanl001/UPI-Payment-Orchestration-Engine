{- |
Module      : Domain.Gateway
Description : Gateway-layer domain types — request/response shapes and errors.

These types sit at the boundary between the orchestration logic and the gateway
implementations. They are intentionally separate from the API-layer types
(no JSON derivations here — that belongs in API.Types).
-}
module Domain.Gateway
  ( -- * Gateway request
    GatewayPaymentRequest (..)
  , GatewayStatusRequest (..)

    -- * Gateway response
  , GatewayPaymentResponse (..)
  , GatewayStatusResponse (..)
  , GatewayStatus (..)

    -- * Gateway error (wraps the failure taxonomy from Transaction)
  , GatewayError (..)

    -- * Gateway metadata (used by router for selection)
  , GatewayMetrics (..)
  , emptyMetrics
  , recordSuccess
  , recordFailure
  , successRate
  ) where

import Data.Text (Text)
import Data.Time (UTCTime, NominalDiffTime)
import GHC.Generics (Generic)

import Domain.Transaction
  ( TransactionId (..)
  , IdempotencyKey (..)
  , VPA (..)
  , Amount (..)
  , GatewayId (..)
  , GatewayFailure (..)
  )

-- ---------------------------------------------------------------------------
-- Gateway request types
-- ---------------------------------------------------------------------------

-- | Everything the gateway needs to process a payment.
data GatewayPaymentRequest = GatewayPaymentRequest
  { reqTxnId       :: !TransactionId
  , reqIdemKey     :: !IdempotencyKey   -- ^ Forwarded to gateway for their dedup too
  , reqPayerVPA    :: !VPA
  , reqPayeeVPA    :: !VPA
  , reqAmount      :: !Amount
  , reqTimestamp   :: !UTCTime
  } deriving stock (Show, Eq, Generic)

-- | Query a gateway for the status of a previously initiated transaction.
data GatewayStatusRequest = GatewayStatusRequest
  { statusReqTxnId   :: !TransactionId
  , statusReqIdemKey :: !IdempotencyKey
  } deriving stock (Show, Eq, Generic)

-- ---------------------------------------------------------------------------
-- Gateway response types
-- ---------------------------------------------------------------------------

-- | Successful initiation response from the gateway.
data GatewayPaymentResponse = GatewayPaymentResponse
  { respGatewayRef   :: !Text          -- ^ Gateway's own transaction ID
  , respGatewayId    :: !GatewayId
  , respProcessedAt  :: !UTCTime
  , respLatencyMs    :: !Int           -- ^ Observed round-trip latency (logged for P99)
  } deriving stock (Show, Eq, Generic)

-- | What a gateway knows about the current state of a transaction.
data GatewayStatus
  = GwSuccess Text   -- ^ Text = gateway reference
  | GwPending        -- ^ Still processing (common in async UPI flows)
  | GwFailed GatewayFailure
  deriving stock (Show, Eq)

data GatewayStatusResponse = GatewayStatusResponse
  { statusRespGwId  :: !GatewayId
  , statusRespStatus :: !GatewayStatus
  , statusRespAt     :: !UTCTime
  } deriving stock (Show, Eq, Generic)

-- ---------------------------------------------------------------------------
-- Gateway error (what a gateway call can return on the Left)
-- ---------------------------------------------------------------------------

{- | The possible outcomes of a gateway call that don't produce a response.

     Note that payment *failures* (InsufficientFunds, InvalidVPA, etc.) are NOT
     here — those are successful gateway responses that happen to say "no".
     GatewayError represents the case where we couldn't even get an answer.
-}
data GatewayError
  = GwTimeout NominalDiffTime    -- ^ Call exceeded the timeout budget
  | GwServerError Int Text       -- ^ HTTP 5xx; Int = status code, Text = body excerpt
  | GwNetworkFailure Text        -- ^ TCP/DNS-level failure
  | GwRateLimited                -- ^ 429 from gateway
  | GwUnexpectedResponse Text    -- ^ We got a response but couldn't parse it
  deriving stock (Show, Eq)

-- ---------------------------------------------------------------------------
-- Gateway metrics — tracked in STM TVar for router decisions
-- ---------------------------------------------------------------------------

{- | Lightweight runtime statistics per gateway.
     The router uses these to rank gateways before each routing decision.

     This is *not* persisted — it resets on restart. A production system
     would back this with a time-series store, but for this portfolio project
     in-memory STM is sufficient to demonstrate the routing pattern.
-}
data GatewayMetrics = GatewayMetrics
  { metricsSuccessCount  :: !Int
  , metricsFailureCount  :: !Int
  , metricsTimeoutCount  :: !Int
  , metricsCurrentLoad   :: !Int    -- ^ Simulated number of in-flight requests
  } deriving stock (Show, Eq, Generic)

-- | Zero-initialised metrics for a freshly registered gateway.
emptyMetrics :: GatewayMetrics
emptyMetrics = GatewayMetrics 0 0 0 0

-- | Record a successful payment on this gateway's metrics.
recordSuccess :: GatewayMetrics -> GatewayMetrics
recordSuccess m = m { metricsSuccessCount = metricsSuccessCount m + 1 }

-- | Record any kind of failure (terminal or retryable).
recordFailure :: GatewayMetrics -> GatewayMetrics
recordFailure m = m { metricsFailureCount = metricsFailureCount m + 1 }

{- | Compute the success rate as a fraction in [0.0, 1.0].
     Returns 1.0 (optimistic) when no attempts have been made yet,
     so a fresh gateway is tried before being penalised.
-}
successRate :: GatewayMetrics -> Double
successRate GatewayMetrics{..} =
  let total = metricsSuccessCount + metricsFailureCount
  in if total == 0
       then 1.0
       else fromIntegral metricsSuccessCount / fromIntegral total
