{- |
Module      : Gateway.MockGatewayA
Description : Mock Gateway A — Fast, high reliability (95% success rate, ~50ms latency).

Models a premium tier gateway like Razorpay or PayU in ideal conditions.
Used as the first-choice gateway in the default router configuration.
-}
module Gateway.MockGatewayA
  ( MockGatewayA (..)
  , mkMockGatewayA
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

-- ---------------------------------------------------------------------------
-- Gateway A data type
-- ---------------------------------------------------------------------------

-- | Opaque handle to Gateway A. Carries its config.
newtype MockGatewayA = MockGatewayA { gwAConfig :: GatewayConfig }

mkMockGatewayA :: MockGatewayA
mkMockGatewayA = MockGatewayA $ defaultConfig (GatewayId "gateway-a")

-- ---------------------------------------------------------------------------
-- PaymentGateway instance
-- ---------------------------------------------------------------------------

instance PaymentGateway MockGatewayA where
  gatewayConfig = gwAConfig

  initiatePayment gw req = ExceptT $ do
    -- Simulate 50ms ± 20ms network latency
    latency <- randomRIO (30_000, 70_000 :: Int)  -- microseconds
    threadDelay latency

    -- 95% success, 3% timeout, 2% server error
    roll <- randomRIO (1, 100 :: Int)
    now  <- getCurrentTime
    ref  <- nextRandom

    pure $ case roll of
      n | n <= 95 ->
          Right GatewayPaymentResponse
            { respGatewayRef  = "GWA-" <> textShow ref
            , respGatewayId   = GatewayId "gateway-a"
            , respProcessedAt = now
            , respLatencyMs   = latency `div` 1000
            }
      n | n <= 98 ->
          Left (GwTimeout 5.0)
      _ ->
          Left (GwServerError 503 "Gateway A temporarily unavailable")

  checkStatus gw req = ExceptT $ do
    threadDelay 20_000  -- status check is faster
    roll <- randomRIO (1, 100 :: Int)
    now  <- getCurrentTime
    ref  <- nextRandom
    pure $ case roll of
      n | n <= 95 ->
          Right GatewayStatusResponse
            { statusRespGwId    = GatewayId "gateway-a"
            , statusRespStatus  = GwSuccess ("GWA-" <> textShow ref)
            , statusRespAt      = now
            }
      _ ->
          Right GatewayStatusResponse
            { statusRespGwId   = GatewayId "gateway-a"
            , statusRespStatus = GwPending
            , statusRespAt     = now
            }


-- ---------------------------------------------------------------------------
-- Internal helper
-- ---------------------------------------------------------------------------

textShow :: Show a => a -> Text
textShow = pack . show

