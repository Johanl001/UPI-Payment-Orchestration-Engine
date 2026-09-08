module Main (main) where

import System.Exit (exitFailure, exitSuccess)

import StateMachineSpec (tests)
import RetrySpec        (tests)
import RouterSpec       (tests)

main :: IO ()
main = do
  r1 <- StateMachineSpec.tests
  r2 <- RetrySpec.tests
  r3 <- RouterSpec.tests
  if r1 && r2 && r3
    then exitSuccess
    else exitFailure
