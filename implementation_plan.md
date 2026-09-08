# UPI Payment Orchestration Engine — Implementation Plan

## Overview

Build **upi-orchestrator**: a production-grade Haskell backend simulating UPI payment routing, 
retry logic, reconciliation, and an auditable ledger — demonstrating idiomatic FP patterns 
used in real payment infrastructure (à la Juspay/EulerHS).

---

## Proposed Changes (7-Step Build)

### Step 1 — Type-Safe State Machine + Core Types

**Key design**: Make illegal state transitions **unrepresentable at the type level** using GADTs 
and phantom types.

#### [NEW] `src/Domain/Transaction.hs`
- `TransactionStatus` ADT: `Initiated | Pending | Success | Failed | TimedOut | Reconciled`
- GADT `Transaction (s :: TransactionStatus)` — phantom-typed so `transition` functions carry 
  type-level proof of valid state pairs
- `TransactionId` (newtype wrapping UUID), `IdempotencyKey` (newtype), `VPA` (newtype), 
  `Amount` (newtype over `Scientific`)
- `FailureReason` ADT split into `RetryableFailure` and `TerminalFailure` — **no string matching**

#### [NEW] `src/Domain/Events.hs`
- `TransactionEvent` sum type representing every possible audit log entry
- `EventId`, `EventTimestamp` newtypes

#### [NEW] `src/Domain/Gateway.hs`
- `GatewayId` newtype, `GatewayResponse` type, `GatewayError` split ADT

---

### Step 2 — Gateway Typeclass + 3 Mock Gateways

#### [NEW] `src/Gateway/Class.hs`
- `PaymentGateway m` typeclass with `initiatePayment`, `checkStatus`, `refund`
- All in `ExceptT GatewayError m` — errors handled structurally

#### [NEW] `src/Gateway/MockGatewayA.hs` — Fast, 95% success rate
#### [NEW] `src/Gateway/MockGatewayB.hs` — Medium latency, 80% success, simulates 5xx
#### [NEW] `src/Gateway/MockGatewayC.hs` — Slow, 70% success, simulates timeouts

---

### Step 3 — Router + Retry Logic

#### [NEW] `src/Orchestrator/Router.hs`
- `RouterConfig` (fallback order, weights)
- Selects gateway based on: success rate (tracked in STM `TVar`), simulated current load
- Returns ordered `NonEmpty GatewayId` for fallback chain

#### [NEW] `src/Orchestrator/Retry.hs`
- `RetryPolicy` type: max attempts, backoff formula, jitter
- `withRetry :: RetryPolicy -> ExceptT RetryableFailure m a -> m (Either FinalError a)`
- Exponential backoff with `threadDelay`
- Idempotency key checked before each attempt against in-memory `TVar (Map IdempotencyKey TxnStatus)`

#### [NEW] `src/Orchestrator/Flow.hs`
- Main orchestration: `runTransaction :: TransactionRequest -> OrchestratorM TransactionResult`
- Uses `ReaderT OrchestratorEnv (ExceptT OrchestratorError IO)` stack

---

### Step 4 — Persistence + Reconciliation

#### [NEW] `src/Persistence/Schema.hs`
- `persistent` schema for `transactions` and `transaction_events` tables
- SQLite backend for dev

#### [NEW] `src/Persistence/Repository.hs`
- `saveTransaction`, `updateTransactionStatus`, `appendEvent`, `getPendingOlderThan`

#### [NEW] `src/Reconciliation/Job.hs`
- Background `async` job: polls Pending txns older than N seconds
- Calls `checkStatus` on originating gateway
- Updates state, appends reconciliation event to audit log

---

### Step 5 — Servant API Layer

#### [NEW] `src/API/Types.hs`
- Request/response JSON types (separate from domain types — no leaking)

#### [NEW] `src/API/Routes.hs`
```
POST /transactions
GET  /transactions/:id
POST /webhook/gateway-callback
GET  /transactions/:id/audit
```

#### [NEW] `src/API/Handlers.hs`
- Handler implementations wired to orchestrator flow

#### [NEW] `app/Main.hs`
- Warp server startup, DB migration, reconciliation job spawn

---

### Step 6 — Tests

#### [NEW] `test/StateMachineSpec.hs` (Hedgehog)
- "No Reconciled transaction can transition to any other state"
- "Idempotency key prevents duplicate Success from same key"

#### [NEW] `test/RetrySpec.hs`
- "Terminal failures are never retried"
- "Exponential backoff intervals are monotonically increasing"

#### [NEW] `test/RouterSpec.hs`
- "Router always returns at least one gateway"
- "Gateway with 0% success rate is never selected as primary"

---

### Step 7 — README + Diagram

#### [NEW] `README.md`
- State machine diagram (ASCII + Mermaid)
- Architecture explanation for non-Haskell readers
- "Why I built this" paragraph

---

## Tech Stack

| Concern | Library |
|---|---|
| Web | `servant`, `servant-server`, `warp` |
| DB | `persistent`, `persistent-sqlite` |
| Concurrency | `stm`, `async` |
| JSON | `aeson` |
| UUIDs | `uuid` |
| Logging | `fast-logger` or `katip` |
| Testing | `hedgehog` |
| Numeric | `scientific` |
| Time | `time` |

---

## Verification Plan

- Each step ends with `stack build` succeeding
- Step 6 ends with `stack test` all green
- Manual curl smoke test against the Servant API
