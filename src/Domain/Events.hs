{- |
Module      : Domain.Events
Description : Append-only audit event log for every transaction state transition.

In real payment systems, the audit log is *immutable* — entries are never updated
or deleted. This module models each event as a typed sum type so pattern matching
on the event history is exhaustive and compiler-checked.

Every call to a state transition function (markPending, markSuccess, etc.) should
produce exactly one TransactionEvent that gets appended to the events table.
-}
module Domain.Events
  ( EventId (..)
  , TransactionEvent (..)
  , EventEnvelope (..)
  , mkEventEnvelope
  ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Text   (Text)
import Data.Time   (UTCTime)
import Data.UUID   (UUID)
import GHC.Generics (Generic)

import Domain.Transaction
  ( TransactionId (..)
  , GatewayId (..)
  , TerminalFailure (..)
  , RetryableFailure (..)
  , ReconciliationOutcome (..)
  )

-- ---------------------------------------------------------------------------
-- Event identity
-- ---------------------------------------------------------------------------

-- | Each event record has its own UUID primary key.
newtype EventId = EventId { unEventId :: UUID }
  deriving stock    (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- ---------------------------------------------------------------------------
-- Event payloads
-- ---------------------------------------------------------------------------

{- | Every possible thing that can happen to a UPI transaction, as a typed ADT.

     The design mirrors event-sourced payment systems: the current state of a
     transaction can always be *reconstructed* by replaying its event history
     in order. This is exactly how Juspay's audit trail works in production.

     We deliberately keep each constructor's payload minimal — only the fields
     that changed in that transition.
-}
data TransactionEvent
  = -- | Transaction was created and queued for processing.
    TxnCreated
      { evtPayerVPA  :: !Text
      , evtPayeeVPA  :: !Text
      , evtAmount    :: !Word    -- ^ In paise
      , evtIdemKey   :: !Text
      }

  | -- | Forwarded to a specific gateway for processing.
    TxnSentToGateway
      { evtGatewayId   :: !GatewayId
      , evtAttemptNum  :: !Int
      }

  | -- | Gateway confirmed the payment was successful.
    TxnSucceededAt
      { evtGatewayId   :: !GatewayId
      , evtGatewayRef  :: !Text    -- ^ Gateway's own transaction reference
      }

  | -- | Payment failed with a terminal reason (will not be retried).
    TxnTerminallyFailed
      { evtGatewayId :: !GatewayId
      , evtReason    :: !TerminalFailure
      }

  | -- | A retryable failure occurred; orchestrator will attempt another gateway.
    TxnRetryableFailure
      { evtGatewayId      :: !GatewayId
      , evtRetryReason    :: !RetryableFailure
      , evtNextAttemptIn  :: !Int          -- ^ Backoff delay in milliseconds
      }

  | -- | Gateway did not respond within the configured timeout window.
    TxnGatewayTimeout
      { evtGatewayId     :: !GatewayId
      , evtTimeoutMillis :: !Int
      }

  | -- | Reconciliation job ran and determined the final outcome.
    TxnReconciled
      { evtOutcome :: !ReconciliationOutcome }

  | -- | Idempotency check short-circuited — returned cached result.
    TxnIdempotentReturn
      { evtCachedStatus :: !Text   -- ^ "Success" | "Failed"
      , evtIdemKey      :: !Text
      }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- ---------------------------------------------------------------------------
-- Envelope — wraps an event with its identity and timestamp
-- ---------------------------------------------------------------------------

{- | The record that gets persisted to the @transaction_events@ table.
     The combination of (txnId, seqNum) is unique — this lets us reconstruct
     the full ordered history of any transaction.
-}
data EventEnvelope = EventEnvelope
  { envEventId    :: !EventId
  , envTxnId      :: !TransactionId
  , envSeqNum     :: !Int          -- ^ Monotonically increasing per transaction
  , envTimestamp  :: !UTCTime
  , envEvent      :: !TransactionEvent
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Construct an event envelope (EventId and seqNum assigned by caller/DB).
mkEventEnvelope
  :: EventId
  -> TransactionId
  -> Int
  -> UTCTime
  -> TransactionEvent
  -> EventEnvelope
mkEventEnvelope = EventEnvelope
