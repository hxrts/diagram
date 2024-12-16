{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON, eitherDecode)
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as B
import ProgramEvaluation (evaluateProgram, scheduleMachine)
import ProgramInstantiation (convertGraph, splitGraphByMachine, Config(..), MachineConfig(..), Graph(..), Notification)

-- Type aliases
type MachineID = String
type GraphConfig = Graph Notification
type SubgraphConfig = (MachineID, Graph Notification)

-- Main function that drives the program, taking a configuration file path as input
main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Usage: Main <config-file-path>"           -- Notify the user about required arguments
    (configFile : _) -> do
      (graphConfig, machineConfigs) <- prepareEnvironment configFile
      let machineSubgraphs = splitGraphByMachine graphConfig
      initializeMachines machineConfigs machineSubgraphs
      evaluateProgram graphConfig machineConfigs

-- Prepare the program graph and machine configs
prepareEnvironment :: FilePath -> IO (GraphConfig, [MachineConfig])
prepareEnvironment configFile = do
  configData <- B.readFile configFile                        -- Read the configuration file as a ByteString
  case eitherDecode configData of                            -- Parse the configuration file into the `Config` type
    Left err -> error $ "Failed to parse config: " ++ err
    Right (Config machineConfigs graphConfig) -> do
      let graph = convertGraph graphConfig                   -- Convert the DAG configuration into a graph representation
          graphConfig = scheduleMachine (map machineID machineConfigs) graph -- Schedule the graph computation across the available machines
      return (graphConfig, machineConfigs)

-- Initialize machines with their respective subgraphs, latencies, and timeouts
initializeMachines :: [MachineConfig] -> [SubgraphConfig] -> IO ()
initializeMachines machineConfigs machineSubgraphs = mapM_ initializeMachine machineSubgraphs
  where
    initializeMachine (machineID, subgraph) = do
      let config = case lookup machineID [(machineID mc, mc) | mc <- machineConfigs] of
                     Just mc -> mc
                     Nothing -> error $ "Configuration not found for machine " ++ machineID
      putStrLn $ "Initializing machine " ++ machineID
      putStrLn $ "Subgraph: " ++ show subgraph
      putStrLn $ "Latency: " ++ show (latency config)
      putStrLn $ "Timeout: " ++ show (timeout config)

-- Evaluate the scheduled program and report outcome
evaluateProgram :: GraphConfig -> [MachineConfig] -> IO ()
evaluateProgram graphConfig machineConfigs = do
  putStrLn "Evaluating distributed computation..."           -- Notify the user about the evaluation process
  results <- evaluateProgram graphConfig machineConfigs  -- Run the evaluation and capture the results
  putStrLn $ case results of                                 -- Print the results based on success or failure
    Just r -> "Computation succeeded: " ++ show r            -- Report success with results
    Nothing -> "Computation failed with rollbacks."          -- Report failure
