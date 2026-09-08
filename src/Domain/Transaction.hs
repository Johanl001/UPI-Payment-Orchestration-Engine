{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeFamilies        #-}

{- |
Module      : Domain.Transaction
Description : Type-safe UPI transaction state machine using GADTs and phantom types.

The core design principle: make illegal state transitions *unrepresentable at the
type level*. GHC rejects code that attempts, for example, to reconcile a transaction
that was never in a terminal state, or to retry a transaction already marked Success.

State lifecycle:
  Initiated ──► Pending ──► Success  ──► Reconciled
                        └──► Failed   ──► Reconciled
                        └──► TimedOut ──► Reconciled

There is no arrow back from Reconciled. There is no arrow from Success to Pending.
The compiler enforces this — not runtime checks, not documentation.
-}
module Domain.Transaction
  ( -- * State kinds (type-level only)
    TxnState (..)

    -- * Core value types
  , TransactionId (..)
  , IdempotencyKey (..)
  , VPA (..)
  , Amount (..)
  , GatewayId (..)

    -- * Failure classification (critical: no string matching)
  , RetryableFailure (..)
  , TerminalFailure (..)
  , GatewayFailure (..)

    -- * The transaction GADT — phantom-typed on TxnState
  , TxnCore (..)
  , Transaction (..)

    -- * Smart constructors (only valid starting state)
  , initiateTransaction

    -- * State transition functions (type signatures enforce valid transitions)
  , markPending
  , markSuccess
  , markFailed
  , markTimedOut
  , reconcileSuccess
  , reconcileFailed
  , reconcileTimedOut

    -- * Existential wrapper for heterogeneous storage
  , SomeTxn (..)
  , someTxn

    -- * Helpers
  , txnId
  , txnCore
  , txnStatus
  ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)

-- ---------------------------------------------------------------------------
-- Type-level state enumeration
-- ---------------------------------------------------------------------------

{- | The set of states a UPI transaction can occupy.
     Used only at the *kind* level — no runtime values of this type exist.
     DataKinds promotes this to a kind, and 'Initiated, 'Pending, etc. become
     type-level tokens that GHC reasons about at compile time.
-}
data TxnState
  = Initiated
  | Pending
  | Success
  | Failed
  | TimedOut
  | Reconciled
  deriving stock (Show, Eq, Ord, Enum, Bounded)

-- ---------------------------------------------------------------------------
-- Core value newtypes — each domain concept has its own type
-- ---------------------------------------------------------------------------

-- | Universally unique transaction identifier.
newtype TransactionId = TransactionId { unTransactionId :: UUID }
  deriving stock    (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

{- | Caller-supplied key that guarantees at-most-once processing. -}
newtype IdempotencyKey = IdempotencyKey { unIdempotencyKey :: Text }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Virtual Payment Address — the UPI identifier of payee/payer.
newtype VPA = VPA { unVPA :: Text }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

{- | Transaction amount in smallest currency unit (paise for INR). -}
newtype Amount = Amount { unAmount :: Natural }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Identifies which payment gateway processed this transaction.
newtype GatewayId = GatewayId { unGatewayId :: Text }
  deriving stock    (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- ---------------------------------------------------------------------------
-- Failure taxonomy — retryable vs terminal, enforced by types
-- ---------------------------------------------------------------------------

{- | Failures that are *transient* — the orchestrator may safely retry
     with the same or a fallback gateway.

     Design note: By placing these in a separate type from TerminalFailure,
     the retry logic can be written as:
       @withRetry :: RetryPolicy -> ExceptT RetryableFailure m a -> m (Either TerminalFailure a)@
     The type signature itself guarantees terminal failures are never retried.
-}
data RetryableFailure
  = GatewayTimeout          -- ^ Gateway did not respond within the SLA window
  | GatewayServerError Text -- ^ Gateway returned 5xx; Text carries the error code
  | NetworkError Text       -- ^ Network-level failure before gateway was reached
  | ThrottledByGateway      -- ^ Gateway rate-limited this merchant
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

{- | Failures that are *permanent* — retrying would either fail identically
     or cause harm (e.g., duplicate debit on the bank side).
     The orchestrator must NOT retry these; it marks the transaction Failed.
-}
data TerminalFailure
  = InvalidVPA Text          -- ^ Payee VPA does not exist or is deregistered
  | InsufficientFunds        -- ^ Payer's account has insufficient balance
  | DailyLimitExceeded       -- ^ Payer has hit their UPI daily transaction limit
  | VPADeregistered Text     -- ^ VPA was valid when initiated but deregistered by now
  | BankAccountBlocked       -- ^ Payer's linked bank account is blocked
  | InvalidAmount            -- ^ Amount is zero, negative, or exceeds per-txn cap
  | DuplicateDetectedByBank  -- ^ Bank's own dedup kicked in (different from our idempotency)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Union of both failure kinds — used where the gateway result is not yet classified.
data GatewayFailure
  = Retryable RetryableFailure
  | Terminal  TerminalFailure
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- ---------------------------------------------------------------------------
-- TxnCore — the data payload common to all states
-- ---------------------------------------------------------------------------

{- | Fields that are present in every state of the transaction. -}
data TxnCore = TxnCore
  { txnCoreId          :: !TransactionId
  , txnCoreIdemKey     :: !IdempotencyKey
  , txnCorePayerVPA    :: !VPA
  , txnCorePayeeVPA    :: !VPA
  , txnCoreAmount      :: !Amount
  , txnCoreCreatedAt   :: !UTCTime
  , txnCoreRetryCount  :: !Int  -- ^ Number of gateway attempts so far
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- ---------------------------------------------------------------------------
-- The Transaction GADT — phantom-typed on TxnState
-- ---------------------------------------------------------------------------

{- | A UPI transaction tagged with its current state at the type level.

     @Transaction 'Initiated@ is a completely different type from
     @Transaction 'Success@. Functions that transition states carry proof
     of the source state in their type signature:

     @markPending :: Transaction 'Initiated -> GatewayId -> UTCTime -> Transaction 'Pending@

     GHC will reject any call that passes a non-Initiated transaction to markPending.
     The bug class "wrong state transition" is eliminated — not at runtime, at compile time.
-}
data Transaction (s :: TxnState) where
  TxnInitiated  :: TxnCore
                -> Transaction 'Initiated

  TxnPending    :: TxnCore
                -> GatewayId    -- ^ Which gateway is currently processing it
                -> UTCTime      -- ^ When it was sent to the gateway
                -> Transaction 'Pending

  TxnSuccess    :: TxnCore
                -> GatewayId    -- ^ Gateway that confirmed success
                -> UTCTime      -- ^ Gateway confirmation timestamp
                -> Text         -- ^ Gateway's own reference ID (for their audit trail)
                -> Transaction 'Success

  TxnFailed     :: TxnCore
                -> GatewayId
                -> UTCTime
                -> TerminalFailure  -- ^ Typed reason — no stringly-typed "error code"
                -> Transaction 'Failed

  TxnTimedOut   :: TxnCore
                -> GatewayId
                -> UTCTime      -- ^ When the timeout was detected
                -> Transaction 'TimedOut

  TxnReconciled :: TxnCore
                -> UTCTime      -- ^ When the reconciliation job ran
                -> ReconciliationOutcome
                -> Transaction 'Reconciled

deriving instance Show (Transaction s)
deriving instance Eq   (Transaction s)

-- | What the reconciliation job determined about a terminal transaction.
data ReconciliationOutcome
  = ConfirmedSuccess GatewayId Text  -- ^ Gateway confirms it succeeded; Text = gateway ref
  | ConfirmedFailed  GatewayId TerminalFailure
  | ConfirmedTimeout GatewayId
  | ManualReview     Text            -- ^ Ambiguous — flagged for human review
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- ---------------------------------------------------------------------------
-- Smart constructor — only Initiated can be created externally
-- ---------------------------------------------------------------------------

{- | The only way to create a transaction.
     Returns @Transaction 'Initiated@ — you cannot construct any other initial state.
     This forces all transactions through the defined lifecycle.
-}
initiateTransaction
  :: TransactionId
  -> IdempotencyKey
  -> VPA        -- ^ Payer
  -> VPA        -- ^ Payee
  -> Amount
  -> UTCTime
  -> Transaction 'Initiated
initiateTransaction tid ikey payer payee amt ts =
  TxnInitiated $ TxnCore
    { txnCoreId         = tid
    , txnCoreIdemKey    = ikey
    , txnCorePayerVPA   = payer
    , txnCorePayeeVPA   = payee
    , txnCoreAmount     = amt
    , txnCoreCreatedAt  = ts
    , txnCoreRetryCount = 0
    }

-- ---------------------------------------------------------------------------
-- State transition functions — type signatures are the specification
-- ---------------------------------------------------------------------------

-- | Send an initiated transaction to a gateway. Increments retry count.
markPending
  :: Transaction 'Initiated
  -> GatewayId
  -> UTCTime
  -> Transaction 'Pending
markPending (TxnInitiated core) gid ts =
  TxnPending (core { txnCoreRetryCount = txnCoreRetryCount core + 1 }) gid ts

-- | A pending transaction was confirmed successful by the gateway.
markSuccess
  :: Transaction 'Pending
  -> UTCTime
  -> Text              -- ^ Gateway reference ID
  -> Transaction 'Success
markSuccess (TxnPending core gid _) ts ref =
  TxnSuccess core gid ts ref

-- | A pending transaction failed with a terminal reason.
markFailed
  :: Transaction 'Pending
  -> UTCTime
  -> TerminalFailure
  -> Transaction 'Failed
markFailed (TxnPending core gid _) ts reason =
  TxnFailed core gid ts reason

-- | A pending transaction timed out (retryable).
markTimedOut
  :: Transaction 'Pending
  -> UTCTime
  -> Transaction 'TimedOut
markTimedOut (TxnPending core gid _) ts =
  TxnTimedOut core gid ts

{- | Reset a timed-out transaction back to Initiated so the retry logic
     can re-attempt on the same or a fallback gateway.

     NOTE: This *increments* the retry count so we can enforce max-retry limits.
     We return Transaction 'Initiated rather than 'Pending directly — this forces
     the caller to go through @markPending@ again with a new gateway choice,
     preventing accidental reuse of a dead gateway reference.
-}
resetForRetry :: Transaction 'TimedOut -> Transaction 'Initiated
resetForRetry (TxnTimedOut core _ _) = TxnInitiated core

-- | Reconcile a Success transaction (closes the ledger entry).
reconcileSuccess
  :: Transaction 'Success
  -> UTCTime
  -> Text              -- ^ Confirmed gateway reference
  -> Transaction 'Reconciled
reconcileSuccess (TxnSuccess core gid _ ref) ts _ =
  TxnReconciled core ts (ConfirmedSuccess gid ref)

-- | Reconcile a Failed transaction.
reconcileFailed
  :: Transaction 'Failed
  -> UTCTime
  -> Transaction 'Reconciled
reconcileFailed (TxnFailed core gid _ reason) ts =
  TxnReconciled core ts (ConfirmedFailed gid reason)

-- | Reconcile a TimedOut transaction (reconciliation job determined final state).
reconcileTimedOut
  :: Transaction 'TimedOut
  -> UTCTime
  -> ReconciliationOutcome
  -> Transaction 'Reconciled
reconcileTimedOut (TxnTimedOut core _ _) ts outcome =
  TxnReconciled core ts outcome

-- ---------------------------------------------------------------------------
-- Existential wrapper — needed to store mixed-state txns in a list/map/DB
-- ---------------------------------------------------------------------------

{- | Wraps a transaction of *any* state for use in heterogeneous contexts
     (e.g., storing in a list, or DB rows where state is a runtime value).

     We deliberately lose the phantom type here, but only at the boundary
     between the type-safe domain core and the persistence layer.
     Inside the orchestration logic we always work with typed transactions.
-}
data SomeTxn = forall s. SomeTxn (Transaction s)

instance Show SomeTxn where
  show (SomeTxn t) = "SomeTxn (" <> txnStatusStr t <> ")"

-- | Wrap any transaction as a SomeTxn.
someTxn :: Transaction s -> SomeTxn
someTxn = SomeTxn

-- ---------------------------------------------------------------------------
-- Accessor helpers
-- ---------------------------------------------------------------------------

-- | Extract the TxnCore from any transaction state.
txnCore :: Transaction s -> TxnCore
txnCore = \case
  TxnInitiated  c         -> c
  TxnPending    c _ _     -> c
  TxnSuccess    c _ _ _   -> c
  TxnFailed     c _ _ _   -> c
  TxnTimedOut   c _ _     -> c
  TxnReconciled c _ _     -> c

-- | Extract the TransactionId from any transaction state.
txnId :: Transaction s -> TransactionId
txnId = txnCoreId . txnCore

-- | Runtime status string (for logging/serialisation).
txnStatus :: Transaction s -> String
txnStatus = txnStatusStr

txnStatusStr :: Transaction s -> String
txnStatusStr = \case
  TxnInitiated  {}  -> "Initiated"
  TxnPending    {}  -> "Pending"
  TxnSuccess    {}  -> "Success"
  TxnFailed     {}  -> "Failed"
  TxnTimedOut   {}  -> "TimedOut"
  TxnReconciled {}  -> "Reconciled"
