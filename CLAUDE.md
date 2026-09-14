# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

**UPI Payment Orchestration Engine** — A production-grade Haskell backend simulating UPI payment routing, retry logic, reconciliation, and an auditable ledger. Built with idiomatic functional programming patterns used in real payment infrastructure (Juspay/EulerHS style).

### Key Architectural Decisions

1. **Type-safe state machine**: GADTs with phantom types make illegal state transitions *unrepresentable at compile time* (see `src/Domain/Transaction.hs`)
2. **Failure taxonomy**: `RetryableFailure` vs `TerminalFailure` split — no string matching, retry logic enforces this at the type level
3. **MTL-style monad stack**: `ReaderT OrchestratorEnv (ExceptT OrchestratorError IO)` for environment + structured errors
4. **STM for concurrency**: In-memory idempotency store, event log, router metrics
5. **Servant API**: Type-level routes with compile-time handler verification

---

## Development Commands

### Build
```bash
stack build                    # Full build
stack build --fast             # Faster incremental build
stack build --ghc-options="-O0" # Debug build
```

### Test
```bash
stack test                     # Run all tests (Hedgehog + tasty)
stack test --ta="-p Router"    # Run only Router tests
stack test --ta="-p Retry"     # Run only Retry tests
stack test --ta="-p StateMachine" # Run only StateMachine tests
```

### Run
```bash
stack run                      # Start server on port 8080
# Server runs at http://localhost:8080
```

### REPL
```bash
stack repl                     # Load library in GHCi
stack repl --no-load           # GHCi without loading project
```

### Lint / Check
```bash
stack build --pedantic         # All warnings as errors
```

---

## Project Structure

```
src/
├── Domain/              # Core domain types (state machine, events, gateway types)
│   ├── Transaction.hs   # GADT-based type-safe state machine
│   ├── Events.hs        # Audit event types
│   └── Gateway.hs       # Gateway request/response/error types
├── Gateway/             # Gateway implementations
│   ├── Class.hs         # PaymentGateway typeclass
│   ├── MockGatewayA.hs  # Fast, 95% success
│   ├── MockGatewayB.hs  # Medium, 80% success, 5xx sim
│   └── MockGatewayC.hs  # Slow, 70% success, timeout sim
├── Orchestrator/        # Core business logic
│   ├── Router.hs        # Gateway selection + success rate tracking (STM)
│   ├── Retry.hs         # Exponential backoff + idempotency checking
│   └── Flow.hs          # Main orchestration entry point
├── Persistence/         # Database layer
│   ├── Schema.hs        # Persistent schema (SQLite)
│   └── Repository.hs    # CRUD + query functions
├── Reconciliation/      # Background job
│   └── Job.hs           # Polls pending txns, calls checkStatus
└── API/                 # Servant HTTP layer
    ├── Types.hs         # Request/response JSON types
    ├── Routes.hs        # Type-level API spec
    └── Handlers.hs      # Handler implementations

app/
└── Main.hs              # Warp server + DB migration + reconciliation job startup

test/
├── Main.hs              # Test runner entry
├── StateMachineSpec.hs  # State machine property tests
├── RetrySpec.hs         # Retry policy property tests
└── RouterSpec.hs        # Router property tests
```

---

## State Machine (Critical)

The transaction lifecycle is enforced at the **type level** via GADTs in `src/Domain/Transaction.hs`:

```
Initiated ──► Pending ──► Success  ──► Reconciled
                    └──► Failed   ──► Reconciled
                    └──► TimedOut ──► Reconciled
```

**No transition back from `Reconciled`**. The compiler rejects invalid transitions — e.g., you cannot call `markSuccess` on a `Transaction 'Failed`.

Key types:
- `Transaction (s :: TxnState)` — phantom-typed on state
- `RetryableFailure` / `TerminalFailure` — disjoint sum types
- `SomeTxn` — existential wrapper for heterogeneous storage (DB boundary only)

---

## API Endpoints

| Method | Path | Description |
|--------|------|-------------|
| POST | `/transactions` | Initiate payment (idempotent) |
| GET | `/transactions/:id` | Get current status |
| POST | `/webhook/gateway-callback` | Async gateway notification |
| GET | `/transactions/:id/audit` | Full event history |

---

## Testing Patterns

- **Property-based tests** using Hedgehog (not example-based)
- Tests live in `test/*.hs` with `prop_` prefix
- Run specific test group: `stack test --ta="-p <GroupName>"`
- Key properties tested:
  - State machine: no invalid transitions, idempotency prevents duplicates
  - Retry: terminal failures never retried, backoff monotonic
  - Router: always returns gateway, success-rate ranking works

---

## Common Tasks

### Add a new gateway
1. Implement `PaymentGateway` instance in `src/Gateway/MockGatewayX.hs`
2. Register in `Orchestrator.Flow.mkOrchestratorEnv` router config
3. Add case in `Orchestrator.Flow.callGateway`

### Modify retry policy
Edit `Orchestrator.Retry.defaultRetryPolicy` — controls max attempts, base delay, max delay, jitter factor.

### Add a new API endpoint
1. Add route type in `API.Routes.UPIOrchestratorAPI`
2. Add request/response types in `API.Types`
3. Implement handler in `API.Handlers`
4. Wire in `API.Routes.upiOrchestratorServer`

### Add a transaction event
1. Add constructor to `TransactionEvent` in `Domain.Events`
2. Add serialization in `Persistence.Schema` if persisted
3. Emit via `appendEvent` in `Orchestrator.Flow`

---

## Tech Stack

| Concern | Library |
|---------|---------|
| Web | `servant`, `servant-server`, `warp` |
| DB | `persistent`, `persistent-sqlite` |
| Concurrency | `stm`, `async` |
| JSON | `aeson` |
| UUIDs | `uuid`, `uuid-types` |
| Testing | `hedgehog`, `tasty`, `tasty-hedgehog` |
| Numeric | `scientific` (via `Amount` newtype) |
| Logging | `fast-logger` |

GHC 9.6.5 (via LTS-22.28), GHC2021 language edition.

---

## Key Files to Understand First

1. `src/Domain/Transaction.hs` — Core state machine, GADT transitions
2. `src/Orchestrator/Flow.hs` — Main orchestration logic, monad stack
3. `src/Orchestrator/Router.hs` — Gateway selection + metrics
4. `src/Orchestrator/Retry.hs` — Retry policy + idempotency
5. `src/API/Routes.hs` — Servant API type
6. `app/Main.hs` — Server startup, wiring