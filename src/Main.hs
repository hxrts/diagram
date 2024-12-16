{-# LANGUAGE OverloadedStrings #-}

module Main where

import ProgramInstantiation (Config(..), MachineConfig(..), ControlFlowGraph(..), convertGraph, splitGraphByMachine)
import ProgramEvaluation (evaluateProgram, scheduleMachine)
import Data.Aeson (eitherDecode)
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as B

-- Type aliases
type ScheduledProgram = ControlFlowGraph String

-- Main function that drives the program, taking a configuration file path as input
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Usage: Main <config-file-path>"           -- Notify the user about required arguments
    (configFile : _) -> do
      (scheduledProgram, machineConfigs) <- prepareEnvironment configFile
      let machineSubgraphs = splitGraphByMachine scheduledProgram
      initializeMachines machineConfigs machineSubgraphs
      evaluateProgram scheduledProgram machineConfigs

-- Prepare the program graph and machine configs
prepareEnvironment :: FilePath -> IO (ScheduledProgram, [MachineConfig])
prepareEnvironment configFile = do
  configData <- B.readFile configFile                        -- Read the configuration file as a ByteString
  case eitherDecode configData of                            -- Parse the configuration file into the `Config` type
    Left err -> error $ "Failed to parse config: " ++ err
    Right (Config machineConfigs dagConfig) -> do
      let graph = convertGraph dagConfig                     -- Convert the DAG configuration into a graph representation
          scheduledProgram = scheduleMachine (map machineID machineConfigs) graph -- Schedule the graph computation across the available machines
      return (scheduledProgram, machineConfigs)

-- Initialize machines with their respective subgraphs and latencies
initializeMachines :: [MachineConfig] -> [(MachineID, ControlFlowGraph String)] -> IO ()
initializeMachines machineConfigs machineSubgraphs = mapM_ initializeMachine machineSubgraphs
  where
    initializeMachine (machineID, subgraph) = do
      let latency = case lookup machineID (map (\mc -> (machineID mc, latency mc)) machineConfigs) of
                      Just l -> l
                      Nothing -> error $ "Latency not found for machine " ++ machineID
      putStrLn $ "Initializing machine " ++ machineID ++ " with subgraph: " ++ show subgraph ++ " and latency: " ++ show latency

-- Evaluate the scheduled program and report outcome
evaluateProgram :: ScheduledProgram -> [MachineConfig] -> IO ()
evaluateProgram scheduledProgram machineConfigs = do
  putStrLn "Evaluating distributed computation..."           -- Notify the user about the evaluation process
  results <- evaluateProgram scheduledProgram machineConfigs  -- Run the evaluation and capture the results
  putStrLn $ case results of                                 -- Print the results based on success or failure
    Just r -> "Computation succeeded: " ++ show r            -- Report success with results
    Nothing -> "Computation failed with rollbacks."          -- Report failure
