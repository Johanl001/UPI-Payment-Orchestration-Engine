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
  , TransactionRecord(TransactionRecord)
  , TransactionRecordId
  -- * Event record
  , EventRecord(EventRecord)
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
import Persistence.SchemaDefs (migrateAll, TransactionRecord(TransactionRecord), TransactionRecordId, EventRecord(EventRecord), EventRecordId)

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
  "INVALID_AMOUNT"     -> "INVALID_AMOUNT"
  "DUPLICATE_DETECTED" -> "DUPLICATE_DETECTED"
  _                    -> "UnknownFailure"
