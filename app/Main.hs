module Main (main) where

import Control.Exception (bracket)
import Database.Persist.Sqlite (runSqlPool, runMigration)
import Network.Wai.Handler.Warp (run)
import Servant (serve)
import System.IO (hPutStrLn, stderr)

import API.Handlers (mkAppEnv, appDbPool, appOrchEnv)
import API.Routes (upiOrchestratorAPI, upiOrchestratorServer)
import Persistence.Schema (migrateAll)
import Reconciliation.Job (startReconciliationJob, defaultReconciliationConfig)

main :: IO ()
main = do
  hPutStrLn stderr "=== UPI Payment Orchestration Engine ==="

  -- Initialise environment (DB pool, orchestrator state, idempotency store)
  env <- mkAppEnv "upi-orchestrator.db"

  -- Run DB migrations (creates tables if not present)
  runSqlPool (runMigration migrateAll) (appDbPool env)
  hPutStrLn stderr "[DB] Migrations applied"

  -- Start background reconciliation job
  _reconcileJob <- startReconciliationJob
                     defaultReconciliationConfig
                     (appDbPool env)
                     (appOrchEnv env)
  hPutStrLn stderr "[Reconciliation] Background job started (interval: 30s)"

  -- Start Warp HTTP server
  let port = 8080
  hPutStrLn stderr $ "[API] Listening on http://localhost:" <> show port
  run port $ serve upiOrchestratorAPI (upiOrchestratorServer env)
