{- |
Module      : Orchestrator.Router
Description : Dynamic gateway selection based on live success metrics.

The router ranks gateways by a weighted score combining:
  1. Historical success rate (from GatewayMetrics in a TVar)
  2. Current load (in-flight count in GatewayMetrics)
  3. Configured static weight (merchant-level preference)

This mirrors how Juspay's payment router works in production: it maintains
a live score per gateway-merchant-bank combination and routes accordingly.
-}
module Orchestrator.Router
  ( RouterConfig (..)
  , RouterEnv (..)
  , mkRouterEnv
  , selectGateway
  , recordGatewayResult
  , GatewayResult (..)
  ) where

import Control.Concurrent.STM
import Data.List (sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ord (comparing, Down (..))

import Domain.Transaction (GatewayId (..))
import Domain.Gateway (GatewayMetrics (..), emptyMetrics, recordSuccess, recordFailure, successRate)

-- ---------------------------------------------------------------------------
-- Configuration
-- ---------------------------------------------------------------------------

-- | Static per-gateway routing weight (higher = more preferred).
data GatewayWeight = GatewayWeight
  { weightGatewayId   :: !GatewayId
  , weightStaticScore :: !Double  -- ^ [0.0 .. 1.0]; controls tie-breaking preference
  } deriving stock (Show, Eq)

data RouterConfig = RouterConfig
  { routerGatewayWeights :: ![GatewayWeight]
    -- ^ Ordered fallback list with static preferences
  , routerLoadPenalty    :: !Double
    -- ^ How much each in-flight request reduces the score (e.g. 0.01)
  } deriving stock (Show, Eq)

defaultRouterConfig :: [GatewayId] -> RouterConfig
defaultRouterConfig gids = RouterConfig
  { routerGatewayWeights = zipWith
      (\gid w -> GatewayWeight gid w)
      gids
      [1.0, 0.8, 0.6]   -- descending static preference
  , routerLoadPenalty = 0.02
  }

-- ---------------------------------------------------------------------------
-- Router environment (lives in IO, shared via ReaderT)
-- ---------------------------------------------------------------------------

-- | Mutable gateway metrics protected by STM — safe for concurrent access.
data RouterEnv = RouterEnv
  { routerConfig  :: !RouterConfig
  , routerMetrics :: !(TVar (Map GatewayId GatewayMetrics))
  }

mkRouterEnv :: [GatewayId] -> IO RouterEnv
mkRouterEnv gids = do
  let cfg     = defaultRouterConfig gids
      initMap = Map.fromList [(gid, emptyMetrics) | gid <- gids]
  tvar <- newTVarIO initMap
  pure $ RouterEnv cfg tvar

-- ---------------------------------------------------------------------------
-- Gateway scoring
-- ---------------------------------------------------------------------------

{- | Score a gateway for routing purposes.

     score = (staticWeight * 0.3) + (successRate * 0.7) - (currentLoad * loadPenalty)

     Weights: success rate is the dominant signal (70%), static preference breaks
     ties (30%). Load penalty prevents overloading a single gateway.
-}
scoreGateway :: RouterConfig -> GatewayMetrics -> Double -> Double
scoreGateway cfg metrics staticW =
  let sr      = successRate metrics
      load    = fromIntegral (metricsCurrentLoad metrics)
      penalty = routerLoadPenalty cfg * load
  in (staticW * 0.3) + (sr * 0.7) - penalty

-- ---------------------------------------------------------------------------
-- Gateway selection
-- ---------------------------------------------------------------------------

{- | Select the ordered list of gateways to try (best first).

     Returns the full ranked list so the caller can implement fallback:
     try index 0, on retryable failure try index 1, etc.

     This is an STM read — consistent snapshot of metrics at the moment of routing.
-}
selectGateway :: RouterEnv -> IO [GatewayId]
selectGateway RouterEnv{..} = do
  metricsMap <- readTVarIO routerMetrics
  let weights = routerGatewayWeights routerConfig
      scored  = flip map weights $ \GatewayWeight{..} ->
        let m     = Map.findWithDefault emptyMetrics weightGatewayId metricsMap
            score = scoreGateway routerConfig m weightStaticScore
        in (weightGatewayId, score)
      ranked = map fst $ sortBy (comparing (Down . snd)) scored
  pure ranked

-- ---------------------------------------------------------------------------
-- Metric updates (called by orchestrator after each gateway attempt)
-- ---------------------------------------------------------------------------

-- | Result of a gateway attempt — used to update metrics.
data GatewayResult = GwResultSuccess | GwResultFailure | GwResultTimeout

-- | Update the live metrics for a gateway after an attempt.
recordGatewayResult :: RouterEnv -> GatewayId -> GatewayResult -> IO ()
recordGatewayResult RouterEnv{..} gid result =
  atomically $ modifyTVar' routerMetrics $ Map.adjust update gid
  where
    update m = case result of
      GwResultSuccess -> recordSuccess m
      GwResultFailure -> recordFailure m
      GwResultTimeout ->
        m { metricsTimeoutCount = metricsTimeoutCount m + 1
          , metricsFailureCount = metricsFailureCount m + 1
          }

-- | Increment/decrement the in-flight counter (for load-aware routing).
adjustLoad :: RouterEnv -> GatewayId -> Int -> STM ()
adjustLoad RouterEnv{..} gid delta =
  modifyTVar' routerMetrics $ Map.adjust
    (\m -> m { metricsCurrentLoad = max 0 (metricsCurrentLoad m + delta) })
    gid
