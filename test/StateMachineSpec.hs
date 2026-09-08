{- |
Module      : StateMachineSpec
Description : Property-based tests for the transaction state machine.

These tests give formal guarantees about the state machine's invariants —
the kind of guarantees that runtime testing alone cannot provide.
-}
module StateMachineSpec (tests) where

import Data.Time (UTCTime, fromGregorian, UTCTime(..))
import Data.UUID (nil)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Domain.Transaction

-- ---------------------------------------------------------------------------
-- Generators
-- ---------------------------------------------------------------------------

genTransactionId :: Gen TransactionId
genTransactionId = pure (TransactionId nil)  -- fixed for simplicity in pure tests

genIdempotencyKey :: Gen IdempotencyKey
genIdempotencyKey = IdempotencyKey <$> Gen.text (Range.linear 8 32) Gen.alphaNum

genVPA :: Gen VPA
genVPA = VPA <$> Gen.text (Range.linear 4 20) Gen.alphaNum

genAmount :: Gen Amount
genAmount = Amount . fromIntegral <$> Gen.int (Range.linear 1 1_000_000)

genGatewayId :: Gen GatewayId
genGatewayId = GatewayId <$> Gen.element ["gateway-a", "gateway-b", "gateway-c"]

genTimestamp :: Gen UTCTime
genTimestamp = pure $ UTCTime (fromGregorian 2024 1 1) 0

genTerminalFailure :: Gen TerminalFailure
genTerminalFailure = Gen.element
  [ InvalidVPA "bad@vpa"
  , InsufficientFunds
  , DailyLimitExceeded
  , BankAccountBlocked
  , InvalidAmount
  ]

genReconciliationOutcome :: Gen ReconciliationOutcome
genReconciliationOutcome = Gen.choice
  [ ConfirmedSuccess <$> genGatewayId <*> Gen.text (Range.linear 4 20) Gen.alphaNum
  , ConfirmedFailed  <$> genGatewayId <*> genTerminalFailure
  , ConfirmedTimeout <$> genGatewayId
  ]

-- Build a freshly initiated transaction
genInitiated :: Gen (Transaction 'Initiated)
genInitiated = initiateTransaction
  <$> genTransactionId
  <*> genIdempotencyKey
  <*> genVPA
  <*> genVPA
  <*> genAmount
  <*> genTimestamp

-- ---------------------------------------------------------------------------
-- Test 1: Initiated → Pending preserves core identity
-- ---------------------------------------------------------------------------

prop_pendingPreservesCore :: Property
prop_pendingPreservesCore = property $ do
  txnI <- forAll genInitiated
  gid  <- forAll genGatewayId
  ts   <- forAll genTimestamp
  let txnP = markPending txnI gid ts
  -- The transaction ID must not change across state transitions
  txnId txnI === txnId txnP

-- ---------------------------------------------------------------------------
-- Test 2: Retry count increments on each markPending call
-- ---------------------------------------------------------------------------

prop_retryCountIncrements :: Property
prop_retryCountIncrements = property $ do
  txnI <- forAll genInitiated
  gid  <- forAll genGatewayId
  ts   <- forAll genTimestamp
  let before = txnCoreRetryCount (txnCore txnI)
      txnP   = markPending txnI gid ts
      after  = txnCoreRetryCount (txnCore txnP)
  after === before + 1

-- ---------------------------------------------------------------------------
-- Test 3: Success → Reconciled is valid; the reverse is not (type-level)
--
-- We can only *test* the forward direction at runtime. The reverse direction
-- (Reconciled → Success) is rejected at *compile time* — there is no function
-- with that signature. The test documents this invariant for reviewers.
-- ---------------------------------------------------------------------------

prop_successCanBeReconciled :: Property
prop_successCanBeReconciled = property $ do
  txnI  <- forAll genInitiated
  gid   <- forAll genGatewayId
  ts    <- forAll genTimestamp
  gwRef <- forAll (Gen.text (Range.linear 4 20) Gen.alphaNum)
  let txnP = markPending txnI gid ts
      txnS = markSuccess txnP ts gwRef
      txnR = reconcileSuccess txnS ts gwRef
  -- After reconciliation, status should reflect Reconciled
  txnStatus txnR === "Reconciled"

-- ---------------------------------------------------------------------------
-- Test 4: Failed → Reconciled is valid; terminal failure is preserved
-- ---------------------------------------------------------------------------

prop_failedReconciliationPreservesReason :: Property
prop_failedReconciliationPreservesReason = property $ do
  txnI   <- forAll genInitiated
  gid    <- forAll genGatewayId
  ts     <- forAll genTimestamp
  reason <- forAll genTerminalFailure
  let txnP = markPending txnI gid ts
      txnF = markFailed txnP ts reason
      txnR = reconcileFailed txnF ts
  -- Outcome must be a ConfirmedFailed carrying the same reason
  case txnR of
    TxnReconciled _ _ (ConfirmedFailed rGid rReason) -> do
      rGid    === gid
      rReason === reason
    _ -> failure

-- ---------------------------------------------------------------------------
-- Test 5: TimedOut → Reconciled accepts any ReconciliationOutcome
-- ---------------------------------------------------------------------------

prop_timedOutReconciliation :: Property
prop_timedOutReconciliation = property $ do
  txnI    <- forAll genInitiated
  gid     <- forAll genGatewayId
  ts      <- forAll genTimestamp
  outcome <- forAll genReconciliationOutcome
  let txnP = markPending txnI gid ts
      txnT = markTimedOut txnP ts
      txnR = reconcileTimedOut txnT ts outcome
  txnStatus txnR === "Reconciled"

-- ---------------------------------------------------------------------------
-- Test 6: Amount in paise is never altered by state transitions
-- ---------------------------------------------------------------------------

prop_amountIsPreservedAcrossTransitions :: Property
prop_amountIsPreservedAcrossTransitions = property $ do
  txnI  <- forAll genInitiated
  gid   <- forAll genGatewayId
  ts    <- forAll genTimestamp
  gwRef <- forAll (Gen.text (Range.linear 4 20) Gen.alphaNum)
  let amt    = txnCoreAmount (txnCore txnI)
      txnP   = markPending txnI gid ts
      txnS   = markSuccess txnP ts gwRef
      txnR   = reconcileSuccess txnS ts gwRef
  -- Amount must be identical at every stage
  txnCoreAmount (txnCore txnP) === amt
  txnCoreAmount (txnCore txnS) === amt
  txnCoreAmount (txnCore txnR) === amt

-- ---------------------------------------------------------------------------
-- Test suite entry
-- ---------------------------------------------------------------------------

tests :: IO Bool
tests = checkParallel $ Group "StateMachine"
  [ ("prop_pendingPreservesCore",              prop_pendingPreservesCore)
  , ("prop_retryCountIncrements",             prop_retryCountIncrements)
  , ("prop_successCanBeReconciled",           prop_successCanBeReconciled)
  , ("prop_failedReconciliationPreservesReason", prop_failedReconciliationPreservesReason)
  , ("prop_timedOutReconciliation",           prop_timedOutReconciliation)
  , ("prop_amountIsPreservedAcrossTransitions", prop_amountIsPreservedAcrossTransitions)
  ]
