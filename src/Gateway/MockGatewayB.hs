{- |
Module      : Gateway.MockGatewayB
Description : Mock Gateway B — Medium reliability (80% success, ~150ms latency, simulates 5xx bursts).

Models a mid-tier gateway with occasional server errors.
Serves as the second fallback in the default router configuration.
-}
module Gateway.MockGatewayB
  ( MockGatewayB (..)
  , mkMockGatewayB
  ) where

import Control.Concurrent (threadDelay)
import Control.Monad.Except (ExceptT (..))
import Data.Text (Text, pack)
import Data.Time (getCurrentTime)
import Data.UUID.V4 (nextRandom)
import System.Random (randomRIO)

import Domain.Gateway
import Domain.Transaction (GatewayId (..))
import Gateway.Class

newtype MockGatewayB = MockGatewayB { gwBConfig :: GatewayConfig }

mkMockGatewayB :: MockGatewayB
mkMockGatewayB = MockGatewayB $ defaultConfig (GatewayId "gateway-b")

instance PaymentGateway MockGatewayB where
  gatewayConfig = gwBConfig

  initiatePayment _ req = ExceptT $ do
    -- Simulate 130ms ± 40ms latency
    latency <- randomRIO (90_000, 170_000 :: Int)
    threadDelay latency

    -- 80% success, 10% 5xx server error, 10% timeout
    roll <- randomRIO (1, 100 :: Int)
    now  <- getCurrentTime
    ref  <- nextRandom

    pure $ case roll of
      n | n <= 80 ->
          Right GatewayPaymentResponse
            { respGatewayRef  = "GWB-" <> textShow ref
            , respGatewayId   = GatewayId "gateway-b"
            , respProcessedAt = now
            , respLatencyMs   = latency `div` 1000
            }
      n | n <= 90 ->
          Left (GwServerError 500 "Gateway B internal server error")
      _ ->
          Left (GwTimeout 5.0)

  checkStatus _ req = ExceptT $ do
    threadDelay 30_000
    roll <- randomRIO (1, 100 :: Int)
    now  <- getCurrentTime
    ref  <- nextRandom
    pure $ case roll of
      n | n <= 80 ->
          Right GatewayStatusResponse
            { statusRespGwId   = GatewayId "gateway-b"
            , statusRespStatus = GwSuccess ("GWB-" <> textShow ref)
            , statusRespAt     = now
            }
      n | n <= 90 ->
          Right GatewayStatusResponse
            { statusRespGwId   = GatewayId "gateway-b"
            , statusRespStatus = GwPending
            , statusRespAt     = now
            }
      _ ->
          Left (GwServerError 500 "Gateway B status check failed")

textShow :: Show a => a -> Text
textShow = pack . show

