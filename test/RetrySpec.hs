{- |
Module      : RetrySpec
Description : Property-based tests for retry logic and idempotency invariants.
-}
module RetrySpec (tests) where

import Control.Concurrent.STM (atomically, modifyTVar', newTVarIO, readTVarIO, TVar)
import qualified Data.Map.Strict as Map
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Domain.Transaction
import Orchestrator.Retry

-- ---------------------------------------------------------------------------
-- Generators
-- ---------------------------------------------------------------------------

genRetryPolicy :: Gen RetryPolicy
genRetryPolicy = do
  maxAttempts <- Gen.int (Range.linear 1 5)
  baseDelay   <- Gen.int (Range.linear 10 100)
  maxDelay    <- Gen.int (Range.linear 200 2000)
  jitter      <- Gen.double (Range.linearFrac 0.0 0.5)
  pure $ RetryPolicy maxAttempts baseDelay maxDelay jitter

genIdempotencyKey :: Gen IdempotencyKey
genIdempotencyKey = IdempotencyKey <$> Gen.text (Range.linear 8 32) Gen.alphaNum

genTransactionId :: Gen TransactionId
genTransactionId = pure $ TransactionId (read "00000000-0000-0000-0000-000000000001")

genTerminalFailure :: Gen TerminalFailure
genTerminalFailure = Gen.element
  [ InsufficientFunds
  , InvalidVPA "test@vpa"
  , DailyLimitExceeded
  , BankAccountBlocked
  , InvalidAmount
  ]

genRetryableFailure :: Gen RetryableFailure
genRetryableFailure = Gen.element
  [ GatewayTimeout
  , GatewayServerError "503"
  , NetworkError "connection refused"
  , ThrottledByGateway
  ]

-- ---------------------------------------------------------------------------
-- Test 1: Terminal failures are NEVER retried
-- ---------------------------------------------------------------------------

prop_terminalFailureNeverRetried :: Property
prop_terminalFailureNeverRetried = property $ do
  policy  <- forAll genRetryPolicy
  failure <- forAll genTerminalFailure

  callCount <- evalIO $ newTVarIO (0 :: Int)
  let action = do
        atomically $ modifyTVar' callCount (+1)
        pure $ Left (Terminal failure)

  outcome <- evalIO $ withRetry policy initialRetryState action
  calls   <- evalIO $ readTVarIO callCount

  calls === 1
  case outcome of
    RetryTerminal _ -> success
    _               -> footnote "Expected RetryTerminal" >> failure

-- ---------------------------------------------------------------------------
-- Test 2: Retryable failures exhaust max attempts exactly
-- ---------------------------------------------------------------------------

prop_retryableExhaustsAttempts :: Property
prop_retryableExhaustsAttempts = property $ do
  policy  <- forAll genRetryPolicy
  retFail <- forAll genRetryableFailure
  let fastPolicy = policy { retryJitterFactor = 0.0, retryBaseDelayMs = 0 }

  callCount <- evalIO $ newTVarIO (0 :: Int)
  let action = do
        atomically $ modifyTVar' callCount (+1)
        pure $ Left (Retryable retFail)

  outcome <- evalIO $ withRetry fastPolicy initialRetryState action
  calls   <- evalIO $ readTVarIO callCount

  calls === retryMaxAttempts fastPolicy
  case outcome of
    RetryExhausted _ _ -> success
    _                  -> footnote "Expected RetryExhausted" >> failure

-- ---------------------------------------------------------------------------
-- Test 3: Idempotency store — recording Success prevents a second Success
-- ---------------------------------------------------------------------------

prop_idempotencyPreventsDoubleSuccess :: Property
prop_idempotencyPreventsDoubleSuccess = property $ do
  key <- forAll genIdempotencyKey
  tid <- forAll genTransactionId
  let gid = GatewayId "gateway-a"

  store <- evalIO newIdempotencyStore
  evalIO $ recordIdempotency store key (IdemSuccess tid gid)

  mEntry <- evalIO $ checkIdempotency store key
  case mEntry of
    Just (IdemSuccess tid2 gid2) -> do
      tid2 === tid
      gid2 === gid
    _ -> footnote "Expected IdemSuccess in store" >> failure

-- ---------------------------------------------------------------------------
-- Test 4: Pending entry is overwriteable by Success
-- ---------------------------------------------------------------------------

prop_pendingEntryOverwrittenBySuccess :: Property
prop_pendingEntryOverwrittenBySuccess = property $ do
  key <- forAll genIdempotencyKey
  tid <- forAll genTransactionId
  let gid = GatewayId "gateway-b"

  store <- evalIO newIdempotencyStore
  evalIO $ recordIdempotency store key (IdemPending tid)
  m1 <- evalIO $ checkIdempotency store key
  case m1 of
    Just (IdemPending _) -> pure ()
    _                    -> footnote "Expected IdemPending" >> failure

  evalIO $ recordIdempotency store key (IdemSuccess tid gid)
  m2 <- evalIO $ checkIdempotency store key
  case m2 of
    Just (IdemSuccess _ _) -> success
    _                      -> footnote "Expected IdemSuccess after overwrite" >> failure

-- ---------------------------------------------------------------------------
-- Test 5: Successful action on first attempt never retries
-- ---------------------------------------------------------------------------

prop_successOnFirstAttemptNeverRetries :: Property
prop_successOnFirstAttemptNeverRetries = property $ do
  policy <- forAll genRetryPolicy
  let fastPolicy = policy { retryJitterFactor = 0.0, retryBaseDelayMs = 0 }

  callCount <- evalIO $ newTVarIO (0 :: Int)
  let action = do
        atomically $ modifyTVar' callCount (+1)
        pure $ Right (42 :: Int)

  outcome <- evalIO $ withRetry fastPolicy initialRetryState action
  calls   <- evalIO $ readTVarIO callCount

  calls === 1
  case outcome of
    RetrySuccess 42 _ -> success
    _                 -> footnote "Expected RetrySuccess 42" >> failure

-- ---------------------------------------------------------------------------
-- Test suite entry
-- ---------------------------------------------------------------------------

tests :: IO Bool
tests = checkParallel $ Group "Retry"
  [ ("prop_terminalFailureNeverRetried",       prop_terminalFailureNeverRetried)
  , ("prop_retryableExhaustsAttempts",         prop_retryableExhaustsAttempts)
  , ("prop_idempotencyPreventsDoubleSuccess",  prop_idempotencyPreventsDoubleSuccess)
  , ("prop_pendingEntryOverwrittenBySuccess",  prop_pendingEntryOverwrittenBySuccess)
  , ("prop_successOnFirstAttemptNeverRetries", prop_successOnFirstAttemptNeverRetries)
  ]
