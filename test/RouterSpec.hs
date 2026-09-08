{- |
Module      : RouterSpec
Description : Property-based tests for the gateway router.
-}
module RouterSpec (tests) where

import Control.Concurrent.STM (readTVarIO)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Domain.Gateway (GatewayMetrics (..), emptyMetrics, successRate)
import Domain.Transaction (GatewayId (..))
import Orchestrator.Router

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

gateways :: [GatewayId]
gateways = [GatewayId "gateway-a", GatewayId "gateway-b", GatewayId "gateway-c"]

-- ---------------------------------------------------------------------------
-- Test 1: Router always returns at least one gateway
-- ---------------------------------------------------------------------------

prop_routerAlwaysReturnsGateway :: Property
prop_routerAlwaysReturnsGateway = property $ do
  env    <- evalIO $ mkRouterEnv gateways
  ranked <- evalIO $ selectGateway env
  assert (not (null ranked))

-- ---------------------------------------------------------------------------
-- Test 2: Router returns exactly the registered gateways
-- ---------------------------------------------------------------------------

prop_routerReturnsAllGateways :: Property
prop_routerReturnsAllGateways = property $ do
  env    <- evalIO $ mkRouterEnv gateways
  ranked <- evalIO $ selectGateway env
  sort ranked === sort gateways

-- ---------------------------------------------------------------------------
-- Test 3: High success rate gateway ranks first
-- ---------------------------------------------------------------------------

prop_highSuccessRateRanksFirst :: Property
prop_highSuccessRateRanksFirst = property $ do
  env <- evalIO $ mkRouterEnv gateways
  let gA = GatewayId "gateway-a"
      gC = GatewayId "gateway-c"

  evalIO $ do
    mapM_ (\_ -> recordGatewayResult env gA GwResultSuccess) [1..95 :: Int]
    mapM_ (\_ -> recordGatewayResult env gC GwResultFailure) [1..95 :: Int]

  ranked <- evalIO $ selectGateway env
  head ranked === gA

-- ---------------------------------------------------------------------------
-- Test 4: Fresh gateway has optimistic success rate of 1.0
-- ---------------------------------------------------------------------------

prop_freshGatewayIsOptimistic :: Property
prop_freshGatewayIsOptimistic = property $ do
  successRate emptyMetrics === 1.0

-- ---------------------------------------------------------------------------
-- Test 5: Success rate increases as successes accumulate
-- ---------------------------------------------------------------------------

prop_successRateMonotonicallyIncreases :: Property
prop_successRateMonotonicallyIncreases = property $ do
  successes <- forAll $ Gen.int (Range.linear 1 100)
  failures  <- forAll $ Gen.int (Range.linear 1 100)
  let m0    = emptyMetrics { metricsFailureCount = failures }
      rate0 = successRate m0
      m1    = m0 { metricsSuccessCount = successes }
      rate1 = successRate m1
  assert (rate1 >= rate0)

-- ---------------------------------------------------------------------------
-- Test 6: recordGatewayResult correctly mutates metrics
-- ---------------------------------------------------------------------------

prop_recordResultUpdatesMetrics :: Property
prop_recordResultUpdatesMetrics = property $ do
  env <- evalIO $ mkRouterEnv gateways
  let gid = GatewayId "gateway-a"

  evalIO $ do
    recordGatewayResult env gid GwResultSuccess
    recordGatewayResult env gid GwResultSuccess
    recordGatewayResult env gid GwResultFailure

  mmap <- evalIO $ readTVarIO (routerMetrics env)
  case Map.lookup gid mmap of
    Nothing -> footnote "Gateway not found in metrics map" >> failure
    Just m  -> do
      metricsSuccessCount m === 2
      metricsFailureCount m === 1

-- ---------------------------------------------------------------------------
-- Test suite entry
-- ---------------------------------------------------------------------------

tests :: IO Bool
tests = checkParallel $ Group "Router"
  [ ("prop_routerAlwaysReturnsGateway",        prop_routerAlwaysReturnsGateway)
  , ("prop_routerReturnsAllGateways",          prop_routerReturnsAllGateways)
  , ("prop_highSuccessRateRanksFirst",         prop_highSuccessRateRanksFirst)
  , ("prop_freshGatewayIsOptimistic",          prop_freshGatewayIsOptimistic)
  , ("prop_successRateMonotonicallyIncreases", prop_successRateMonotonicallyIncreases)
  , ("prop_recordResultUpdatesMetrics",        prop_recordResultUpdatesMetrics)
  ]
