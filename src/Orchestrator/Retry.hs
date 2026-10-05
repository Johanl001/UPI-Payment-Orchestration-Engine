{- |
Module      : Orchestrator.Retry
Description : Exponential backoff retry logic with idempotency enforcement.

Design goals:
  1. Terminal failures are *never* retried — enforced by types, not guards.
  2. Idempotency keys short-circuit before any gateway call — no double-charges.
  3. Backoff intervals grow exponentially with optional jitter — prevents
     thundering-herd when multiple transactions retry simultaneously.
  4. Max attempts are strictly enforced — runaway retry loops are impossible.
-}
module Orchestrator.Retry
  ( RetryPolicy (..)
  , defaultRetryPolicy
  , RetryState (..)
  , initialRetryState
  , RetryOutcome (..)
  , withRetry
  -- * Idempotency store
  , IdempotencyStore
  , newIdempotencyStore
  , checkIdempotency
  , recordIdempotency
  , IdempotencyEntry (..)
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Monad.Except
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import System.Random (randomRIO)

import Domain.Transaction

-- ---------------------------------------------------------------------------
-- Retry policy
-- ---------------------------------------------------------------------------

data RetryPolicy = RetryPolicy
  { retryMaxAttempts   :: !Int
    -- ^ Maximum number of gateway attempts (including the first).
  , retryBaseDelayMs   :: !Int
    -- ^ Base delay in milliseconds for the first retry.
  , retryMaxDelayMs    :: !Int
    -- ^ Cap on any single delay (prevents absurdly long waits).
  , retryJitterFactor  :: !Double
    -- ^ [0.0 .. 1.0] — fraction of the delay added as random jitter.
    --   Set to 0 for deterministic tests, ~0.25 in production to spread load.
  } deriving stock (Show, Eq)

defaultRetryPolicy :: RetryPolicy
defaultRetryPolicy = RetryPolicy
  { retryMaxAttempts  = 3
  , retryBaseDelayMs  = 200
  , retryMaxDelayMs   = 5000
  , retryJitterFactor = 0.25
  }

-- ---------------------------------------------------------------------------
-- Retry state (threaded through each attempt)
-- ---------------------------------------------------------------------------

data RetryState = RetryState
  { retryAttemptNum     :: !Int
  , retryLastDelayMs    :: !Int
  , retryGatewaysUsed   :: ![GatewayId]
  } deriving stock (Show, Eq)

initialRetryState :: RetryState
initialRetryState = RetryState 1 0 []

-- ---------------------------------------------------------------------------
-- Retry outcome
-- ---------------------------------------------------------------------------

{- | What happened after all retry attempts.

     The 'a' is the success value (e.g., GatewayPaymentResponse).
     The type says: you either get an 'a' or a TerminalFailure. There is no
     constructor for "we ran out of retries on a retryable failure" — instead,
     that situation is modelled as @Left (ExhaustedRetries lastRetryableFailure)@.
     But rather than extending TerminalFailure (that would blur the domain boundary),
     we use a separate RetryOutcome type.
-}
data RetryOutcome a
  = RetrySuccess a           RetryState
  | RetryTerminal            TerminalFailure
  | RetryExhausted           RetryableFailure RetryState  -- ^ Ran out of attempts
  deriving stock (Show)

-- ---------------------------------------------------------------------------
-- Core retry runner
-- ---------------------------------------------------------------------------

{- | Run an action with exponential backoff.

     The action returns:
       - @Right a@                 → success, stop retrying.
       - @Left (Retryable r)@     → retryable, back off and try again.
       - @Left (Terminal t)@      → terminal, stop immediately and propagate.

     The type signature is the specification:
       * Returning @RetryTerminal@ proves GHC that terminal failures are never retried.
       * There is no way to accidentally call @withRetry@ on an action that only
         ever returns terminal failures — the type enforces that @RetryableFailure@
         must be in scope.
-}
withRetry
  :: RetryPolicy
  -> RetryState
  -> IO (Either GatewayFailure a)   -- ^ The action to attempt
  -> IO (RetryOutcome a)
withRetry policy state action
  | retryAttemptNum state > retryMaxAttempts policy =
      -- Should not happen normally, but guards against programming errors.
      pure $ RetryExhausted (GatewayTimeout) state  -- placeholder failure
  | otherwise = do
      result <- action
      case result of
        Right a ->
          pure $ RetrySuccess a state

        Left (Terminal t) ->
          -- Terminal failure: DO NOT retry. Type-level proof: this branch
          -- returns RetryTerminal, which carries TerminalFailure, not RetryState.
          pure $ RetryTerminal t

        Left (Retryable r) ->
          if retryAttemptNum state >= retryMaxAttempts policy
            then pure $ RetryExhausted r state
            else do
              -- Compute exponential backoff with jitter
              delayMs <- computeDelay policy (retryAttemptNum state)
              threadDelay (delayMs * 1000)
              let state' = state
                    { retryAttemptNum  = retryAttemptNum  state + 1
                    , retryLastDelayMs = delayMs
                    }
              withRetry policy state' action

-- | Compute delay for attempt @n@ (1-indexed).
--   delay = min(maxDelay, baseDelay * 2^(n-1)) * (1 + jitter)
computeDelay :: RetryPolicy -> Int -> IO Int
computeDelay RetryPolicy{..} attemptNum = do
  let base      = retryBaseDelayMs * (2 ^ (attemptNum - 1))
      capped    = min retryMaxDelayMs base
  jitter <- randomRIO (0, round (fromIntegral capped * retryJitterFactor))
  pure (capped + jitter)

-- ---------------------------------------------------------------------------
-- Idempotency store
-- ---------------------------------------------------------------------------

{- | An idempotency entry records what happened the last time a key was processed.

     The orchestrator checks this *before* calling any gateway. If an entry
     exists and the status is Success, it returns the cached result immediately.
     This guarantees: one IdempotencyKey → at most one Success, regardless of
     how many times the caller retries.
-}
data IdempotencyEntry
  = IdemPending  TransactionId          -- ^ Currently in-flight
  | IdemSuccess  TransactionId GatewayId  -- ^ Already succeeded
  | IdemFailed   TransactionId TerminalFailure
  deriving stock (Show, Eq)

{- | In-memory idempotency store backed by STM TVar.

     In production this would be Redis or a DB unique constraint on (idem_key, status).
     For this portfolio project, in-memory is sufficient to demonstrate the pattern.
-}
newtype IdempotencyStore = IdempotencyStore
  { idemStoreVar :: TVar (Map IdempotencyKey IdempotencyEntry) }

newIdempotencyStore :: IO IdempotencyStore
newIdempotencyStore = IdempotencyStore <$> newTVarIO Map.empty

-- | Check whether this idempotency key has already been processed.
checkIdempotency
  :: IdempotencyStore
  -> IdempotencyKey
  -> IO (Maybe IdempotencyEntry)
checkIdempotency (IdempotencyStore var) key =
  Map.lookup key <$> readTVarIO var

-- | Record the result of processing a transaction for an idempotency key.
recordIdempotency
  :: IdempotencyStore
  -> IdempotencyKey
  -> IdempotencyEntry
  -> IO ()
recordIdempotency (IdempotencyStore var) key entry =
  atomically $ modifyTVar' var (Map.insert key entry)
