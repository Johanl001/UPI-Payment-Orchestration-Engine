{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

{- |
Module      : Persistence.SchemaDefs
Description : Contains the Template Haskell code for generating the Persistent schema.

This module has NO explicit export list so that all TH-generated definitions
(including EntityField proxies) are implicitly exported. This works around
the limitation that TH-generated names cannot be explicitly listed in an
export list because the export list is processed before the TH splices run.
-}
module Persistence.SchemaDefs where

import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist.TH

-- | Persistent schema definition for transactions and the audit event log.
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