{- |
Module      : Persistence.Repository
Description : Data access layer — the only module that touches the DB.

Design rule: Domain types never leak into SQL; SQL types never leak into domain.
Every function converts at the boundary. This means the persistence layer can be
swapped (SQLite → Postgres) without touching any other module.
-}
module Persistence.Repository
  ( DbPool
  , mkPool
  , saveTransaction
  , updateTransactionStatus
  , getTransaction
  , getPendingOlderThan
  , appendEvent
  , getEventHistory
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (runNoLoggingT)
import Data.Aeson (encode, toJSON)
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy (toStrict)
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8)
import Data.Time (UTCTime, getCurrentTime, addUTCTime, NominalDiffTime)
import Data.UUID (toText)
import Database.Persist.Sqlite
import Database.Persist.TH ()

import Domain.Events (EventEnvelope (..), EventId (..), TransactionEvent (..))
import Domain.Gateway
import Domain.Transaction
import Persistence.Schema
import Persistence.SchemaDefs

-- ---------------------------------------------------------------------------
-- Connection pool
-- ---------------------------------------------------------------------------

type DbPool = ConnectionPool

-- | Create a SQLite connection pool.
mkPool :: Text -> IO DbPool
mkPool dbPath = runNoLoggingT $ createSqlitePool dbPath 10

-- ---------------------------------------------------------------------------
-- Transaction CRUD
-- ---------------------------------------------------------------------------

-- | Insert a newly created transaction into the DB.
saveTransaction
  :: DbPool
  -> Transaction 'Initiated
  -> IO ()
saveTransaction pool txn = do
  now <- getCurrentTime
  let core = txnCore txn
  runSqlPool (insert_ rec) pool
  where
    core = txnCore txn
    rec  = TransactionRecord
      { transactionRecordTxnId          = toText (unTransactionId (txnCoreId core))
      , transactionRecordIdempotencyKey = unIdempotencyKey (txnCoreIdemKey core)
      , transactionRecordPayerVpa       = unVPA (txnCorePayerVPA core)
      , transactionRecordPayeeVpa       = unVPA (txnCorePayeeVPA core)
      , transactionRecordAmountPaise    = fromIntegral (unAmount (txnCoreAmount core))
      , transactionRecordStatus         = "Initiated"
      , transactionRecordGatewayId      = Nothing
      , transactionRecordGatewayRef     = Nothing
      , transactionRecordFailureReason  = Nothing
      , transactionRecordRetryCount     = 0
      , transactionRecordCreatedAt      = txnCoreCreatedAt core
      , transactionRecordUpdatedAt      = txnCoreCreatedAt core
      }

-- | Update a transaction's status after a state transition.
updateTransactionStatus
  :: DbPool
  -> TransactionId
  -> Text             -- ^ New status
  -> Maybe GatewayId
  -> Maybe Text       -- ^ Gateway reference
  -> Maybe Text       -- ^ Failure reason
  -> Int              -- ^ Retry count
  -> IO ()
updateTransactionStatus pool tid status mgid mref mfail retries = do
  now <- getCurrentTime
  let tidText = toText (unTransactionId tid)
  runSqlPool
    (updateWhere
       [TransactionRecordTxnId ==. tidText]
       [ TransactionRecordStatus         =. status
       , TransactionRecordGatewayId      =. fmap unGatewayId mgid
       , TransactionRecordGatewayRef     =. mref
       , TransactionRecordFailureReason  =. mfail
       , TransactionRecordRetryCount     =. retries
       , TransactionRecordUpdatedAt      =. now
       ])
    pool

-- | Fetch a single transaction by ID.
getTransaction
  :: DbPool
  -> TransactionId
  -> IO (Maybe TransactionRecord)
getTransaction pool tid = do
  let tidText = toText (unTransactionId tid)
  fmap (fmap entityVal) $ runSqlPool
    (selectFirst [TransactionRecordTxnId ==. tidText] [])
    pool

-- | Return all Pending transactions older than @age@ seconds.
--   Used by the reconciliation job.
getPendingOlderThan
  :: DbPool
  -> NominalDiffTime   -- ^ Age threshold in seconds
  -> IO [TransactionRecord]
getPendingOlderThan pool age = do
  now <- getCurrentTime
  let cutoff = addUTCTime (negate age) now
  fmap (map entityVal) $ runSqlPool
    (selectList
       [ TransactionRecordStatus   ==. "Pending"
       , TransactionRecordCreatedAt <. cutoff
       ]
       [Asc TransactionRecordCreatedAt])
    pool

-- ---------------------------------------------------------------------------
-- Audit event log (append-only)
-- ---------------------------------------------------------------------------

-- | Append an event envelope to the DB (INSERT only — no UPDATE ever).
appendEvent :: DbPool -> EventEnvelope -> IO ()
appendEvent pool env = do
  let payload = decodeUtf8 . toStrict . encode . toJSON $ envEvent env
      rec = EventRecord
        { eventRecordEventId   = toText (unEventId (envEventId env))
        , eventRecordTxnId     = toText (unTransactionId (envTxnId env))
        , eventRecordSeqNum    = envSeqNum env
        , eventRecordEventType = eventTypeName (envEvent env)
        , eventRecordEventPayload = payload
        , eventRecordCreatedAt = envTimestamp env
        }
  runSqlPool (insert_ rec) pool

-- | Retrieve the full event history for a transaction, ordered by seqNum.
getEventHistory :: DbPool -> TransactionId -> IO [EventRecord]
getEventHistory pool tid = do
  let tidText = toText (unTransactionId tid)
  fmap (map entityVal) $ runSqlPool
    (selectList
       [EventRecordTxnId ==. tidText]
       [Asc EventRecordSeqNum])
    pool

-- ---------------------------------------------------------------------------
-- Internal helpers
-- ---------------------------------------------------------------------------

eventTypeName :: TransactionEvent -> Text
eventTypeName = \case
  TxnCreated{}           -> "TxnCreated"
  TxnSentToGateway{}     -> "TxnSentToGateway"
  TxnSucceededAt{}       -> "TxnSucceededAt"
  TxnTerminallyFailed{}  -> "TxnTerminallyFailed"
  TxnRetryableFailure{}  -> "TxnRetryableFailure"
  TxnGatewayTimeout{}    -> "TxnGatewayTimeout"
  Domain.Events.TxnReconciled{}        -> "TxnReconciled"
  TxnIdempotentReturn{}  -> "TxnIdempotentReturn"
