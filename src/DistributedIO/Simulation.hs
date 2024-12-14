module DistributedIO.Simulation
  ( simulateMachine,
  ) where

import DistributedIO.Core (MachineID)

-- Simulate running a computation on a specific machine
simulateMachine :: MachineID -> String -> IO String
simulateMachine machineID input = do
  putStrLn $ "Machine " ++ machineID ++ " processing: " ++ input
  return $ "Output from " ++ machineID ++ " for input: " ++ input
