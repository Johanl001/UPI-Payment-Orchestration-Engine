{- |
Module      : Gateway.Class
Description : The PaymentGateway typeclass — the common interface all gateways implement.

Why a typeclass instead of a plain record of functions?

  * Each mock gateway is its own *type* (MockGatewayA, MockGatewayB, MockGatewayC).
    This lets us write @processPayment @MockGatewayA req@ — the gateway choice is
    visible in the type, not hidden in a runtime value.
  * In tests we can define an @instance PaymentGateway TestGateway@ that is
    completely deterministic — no IO, no randomness.
  * The constraint @PaymentGateway g => ...@ documents gateway dependencies
    without passing gateway "handle" arguments everywhere.

The monad is polymorphic (@m@) so the typeclass works in IO (production),
STM transactions (concurrent tests), and pure tests (Identity monad).
-}
module Gateway.Class
  ( PaymentGateway (..)
  , GatewayConfig (..)
  , defaultConfig
  ) where

import Control.Monad.Except (ExceptT)
import Data.Text (Text)

import Domain.Gateway
  ( GatewayPaymentRequest (..)
  , GatewayPaymentResponse (..)
  , GatewayStatusRequest (..)
  , GatewayStatusResponse (..)
  , GatewayError (..)
  )
import Domain.Transaction (GatewayId (..))

-- ---------------------------------------------------------------------------
-- Gateway configuration
-- ---------------------------------------------------------------------------

-- | Static configuration for a gateway instance.
data GatewayConfig = GatewayConfig
  { gwConfigId          :: !GatewayId
  , gwConfigTimeoutMs   :: !Int    -- ^ How long to wait before declaring GwTimeout
  , gwConfigEndpoint    :: !Text   -- ^ Mock "URL" — used in logs for traceability
  } deriving stock (Show, Eq)

defaultConfig :: GatewayId -> GatewayConfig
defaultConfig gid = GatewayConfig
  { gwConfigId        = gid
  , gwConfigTimeoutMs = 5000
  , gwConfigEndpoint  = "https://mock-gateway/" <> unGatewayId gid
  }

-- ---------------------------------------------------------------------------
-- The typeclass
-- ---------------------------------------------------------------------------

{- | A payment gateway that can initiate payments and check their status.

     Laws (not enforced by the compiler, but tested via property tests):

     1. *Idempotency*: Calling @initiatePayment@ twice with the same
        @IdempotencyKey@ MUST return the same result — not a duplicate charge.

     2. *Status consistency*: If @initiatePayment@ returns a success response,
        @checkStatus@ with the same transaction ID must eventually return
        @GwSuccess@ (convergence guarantee).

     3. *No phantom success*: A gateway that returns @GwSuccess@ from
        @checkStatus@ must have actually processed the payment.
-}
class PaymentGateway g where
  -- | The configuration record associated with this gateway.
  gatewayConfig :: g -> GatewayConfig

  {- | Initiate a payment. Returns either a typed error (Left) or a
       success response (Right). Note that a "payment declined" result
       is *not* an error here — it comes back as a successful response
       with the failure reason encoded in the response body (modelled
       separately by the caller parsing the response into a GatewayFailure).

       In this mock, we fold that together for simplicity: the mock returns
       Either GatewayError GatewayPaymentResponse, and the orchestrator
       interprets a Left as a retryable gateway-level failure.
  -}
  initiatePayment
    :: g
    -> GatewayPaymentRequest
    -> ExceptT GatewayError IO GatewayPaymentResponse

  {- | Check the current status of a transaction. Used by the reconciliation job
       for Pending transactions that didn't receive a callback.
  -}
  checkStatus
    :: g
    -> GatewayStatusRequest
    -> ExceptT GatewayError IO GatewayStatusResponse
