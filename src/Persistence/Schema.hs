{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

{- |
Module      : Persistence.Schema
Description : Persistent ORM schema for transactions and the audit event log.

Two tables:
  * @transactions@ — current state of each UPI transaction (mutable row).
  * @transaction_events@ — append-only audit log (INSERT only, never UPDATE/DELETE).

The mutable @transactions@ table is a projection / current-state cache.
The @transaction_events@ table is the source of truth — in an event-sourced
system we could reconstruct the full transaction state from events alone.
Both are kept for pragmatic reasons: the mutable row makes status queries O(1).
-}
module Persistence.Schema
  ( migrateAll
  -- * Transaction record
  , TransactionRecord (..)
  , TransactionRecordId
  -- * Event record
  , EventRecord (..)
  , EventRecordId
  -- * Status/failure encodings (Text round-trips)
  , encodeStatus
  , decodeStatus
  , encodeTerminalFailure
  , decodeTerminalFailure
  ) where

import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist.TH

-- ---------------------------------------------------------------------------
-- Persistent schema definition
-- ---------------------------------------------------------------------------

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|

-- | Current state of each UPI transaction.
TransactionRecord
  txnId           Text          -- UUID as text (primary business key)
  idempotencyKey  Text
  payerVpa        Text
  payeeVpa        Text
  amountPaise     Int
  status          Text          -- "Initiated"|"Pending"|"Success"|"Failed"|"TimedOut"|"Reconciled"
  gatewayId       Text Maybe    -- NULL until sent to a gateway
  gatewayRef      Text Maybe    -- NULL until gateway confirms
  failureReason   Text Maybe    -- NULL unless Failed
  retryCount      Int
  createdAt       UTCTime
  updatedAt       UTCTime
  UniqueIdempotencyKey idempotencyKey  -- DB-level uniqueness guarantee
  deriving Show

-- | Append-only audit event log — never UPDATE or DELETE rows here.
EventRecord
  eventId         Text          -- UUID as text
  txnId           Text          -- FK to TransactionRecord.txnId (by business key)
  seqNum          Int           -- Monotonically increasing per txnId
  eventType       Text          -- e.g. "TxnCreated", "TxnSentToGateway", ...
  eventPayload    Text          -- JSON-encoded event payload
  createdAt       UTCTime
  deriving Show

|]

-- ---------------------------------------------------------------------------
-- Text encodings for status and failure (no orphan instances needed)
-- ---------------------------------------------------------------------------

encodeStatus :: String -> Text
encodeStatus = \s -> case s of
  "Initiated"  -> "Initiated"
  "Pending"    -> "Pending"
  "Success"    -> "Success"
  "Failed"     -> "Failed"
  "TimedOut"   -> "TimedOut"
  "Reconciled" -> "Reconciled"
  other        -> "Unknown"

decodeStatus :: Text -> String
decodeStatus t = case t of
  "Initiated"  -> "Initiated"
  "Pending"    -> "Pending"
  "Success"    -> "Success"
  "Failed"     -> "Failed"
  "TimedOut"   -> "TimedOut"
  "Reconciled" -> "Reconciled"
  _            -> "Unknown"

encodeTerminalFailure :: String -> Text
encodeTerminalFailure s = case s of
  "InvalidVPA"              -> "INVALID_VPA"
  "InsufficientFunds"       -> "INSUFFICIENT_FUNDS"
  "DailyLimitExceeded"      -> "DAILY_LIMIT_EXCEEDED"
  "VPADeregistered"         -> "VPA_DEREGISTERED"
  "BankAccountBlocked"      -> "BANK_ACCOUNT_BLOCKED"
  "InvalidAmount"           -> "INVALID_AMOUNT"
  "DuplicateDetectedByBank" -> "DUPLICATE_DETECTED"
  _                         -> "UNKNOWN_FAILURE"

decodeTerminalFailure :: Text -> String
decodeTerminalFailure t = case t of
  "INVALID_VPA"        -> "InvalidVPA"
  "INSUFFICIENT_FUNDS" -> "InsufficientFunds"
  "DAILY_LIMIT_EXCEEDED" -> "DailyLimitExceeded"
  "VPA_DEREGISTERED"   -> "VPADeregistered"
  "BANK_ACCOUNT_BLOCKED" -> "BankAccountBlocked"
  "INVALID_AMOUNT"     -> "InvalidAmount"
  "DUPLICATE_DETECTED" -> "DuplicateDetectedByBank"
  _                    -> "UnknownFailure"
