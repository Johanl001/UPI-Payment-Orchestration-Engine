{- |
Module      : API.Routes
Description : Servant API type-level route definitions.

Why Servant?
  Servant encodes the API as a *type*. Client code, server handlers, and
  documentation are all derived from the same type-level spec — so if a handler
  returns the wrong type, it's a compile error, not a runtime 404.

  This mirrors how Juspay uses Servant internally for their HTTP surface.
-}
module API.Routes
  ( UPIOrchestratorAPI
  , upiOrchestratorAPI
  , upiOrchestratorServer
  ) where

import Data.Text (Text)
import Servant

import API.Types
import API.Handlers (AppEnv, initiateHandler, statusHandler, callbackHandler, auditHandler)

-- ---------------------------------------------------------------------------
-- API type
-- ---------------------------------------------------------------------------

{- | The full API as a Servant type-level spec.

     Reading this type is reading the API contract:
       POST /transactions           → initiates a payment
       GET  /transactions/:id       → current status
       POST /webhook/gateway-callback → async gateway notification
       GET  /transactions/:id/audit → full event history
-}
type UPIOrchestratorAPI
  =    "transactions"
         :> ReqBody '[JSON] InitiateRequest
         :> Post    '[JSON] InitiateResponse

  :<|> "transactions"
         :> Capture "txnId" Text
         :> Get '[JSON] TransactionStatusResponse

  :<|> "webhook"
         :> "gateway-callback"
         :> ReqBody '[JSON] GatewayCallbackPayload
         :> Post    '[JSON] GatewayCallbackResponse

  :<|> "transactions"
         :> Capture "txnId" Text
         :> "audit"
         :> Get '[JSON] AuditHistoryResponse

-- ---------------------------------------------------------------------------
-- Proxy and server
-- ---------------------------------------------------------------------------

upiOrchestratorAPI :: Proxy UPIOrchestratorAPI
upiOrchestratorAPI = Proxy

upiOrchestratorServer :: AppEnv -> Server UPIOrchestratorAPI
upiOrchestratorServer env
  =    initiateHandler env
  :<|> statusHandler   env
  :<|> callbackHandler env
  :<|> auditHandler    env
