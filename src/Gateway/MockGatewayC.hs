{- |
Module      : Gateway.MockGatewayC
Description : Mock Gateway C — Slow, low reliability (70% success, ~300ms latency, frequent timeouts).

Models a legacy or congested gateway — used as last resort.
Its high timeout rate exercises the reconciliation job path (transactions
that time out and must be reconciled asynchronously).
-}
module Gateway.MockGatewayC
  ( MockGatewayC (..)
  , mkMockGatewayC
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad.Except (ExceptT (..))
import Data.Text (Text, pack)
import Data.Time (getCurrentTime)
import Data.UUID.V4 (nextRandom)
import System.Random (randomRIO)

import Domain.Gateway
import Domain.Transaction (GatewayId (..), TerminalFailure (..), GatewayFailure (..))
import Gateway.Class

newtype MockGatewayC = MockGatewayC { gwCConfig :: GatewayConfig }

mkMockGatewayC :: MockGatewayC
mkMockGatewayC = MockGatewayC $ defaultConfig (GatewayId "gateway-c")

instance PaymentGateway MockGatewayC where
  gatewayConfig = gwCConfig

  initiatePayment _ req = ExceptT $ do
    -- Simulate 260ms ± 80ms latency
    latency <- randomRIO (180_000, 340_000 :: Int)
    threadDelay latency

    -- 70% success, 20% timeout, 7% server error, 3% terminal (insufficient funds)
    roll <- randomRIO (1, 100 :: Int)
    now  <- getCurrentTime
    ref  <- nextRandom

    pure $ case roll of
      n | n <= 70 ->
          Right GatewayPaymentResponse
            { respGatewayRef  = "GWC-" <> textShow ref
            , respGatewayId   = GatewayId "gateway-c"
            , respProcessedAt = now
            , respLatencyMs   = latency `div` 1000
            }
      n | n <= 90 ->
          Left (GwTimeout 5.0)
      n | n <= 97 ->
          Left (GwServerError 503 "Gateway C overloaded")
      _ ->
          -- Gateway C occasionally surfaces terminal failures that it detects
          -- (e.g., it checks the bank before we do)
          -- We encode this as a server error with a special code — the orchestrator
          -- parses known codes into TerminalFailure values.
          Left (GwServerError 422 "INSUFFICIENT_FUNDS")

  checkStatus _ req = ExceptT $ do
    -- Gateway C status check is also slow
    threadDelay 80_000
    roll <- randomRIO (1, 100 :: Int)
    now  <- getCurrentTime
    ref  <- nextRandom
    pure $ case roll of
      n | n <= 70 ->
          Right GatewayStatusResponse
            { statusRespGwId   = GatewayId "gateway-c"
            , statusRespStatus = GwSuccess ("GWC-" <> textShow ref)
            , statusRespAt     = now
            }
      n | n <= 85 ->
          Right GatewayStatusResponse
            { statusRespGwId   = GatewayId "gateway-c"
            , statusRespStatus = GwPending    -- Still processing — reconciliation will retry
            , statusRespAt     = now
            }
      n | n <= 95 ->
          Right GatewayStatusResponse
            { statusRespGwId   = GatewayId "gateway-c"
            , statusRespStatus = GwFailed (Terminal InsufficientFunds)
            , statusRespAt     = now
            }
      _ ->
          Left (GwTimeout 5.0)

textShow :: Show a => a -> Text
textShow = pack . show

