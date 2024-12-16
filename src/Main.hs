{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON, eitherDecode)
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy as B
import ProgramEvaluation (evaluateProgram, scheduleMachine)
import ProgramInstantiation (convertGraph, splitGraphByMachine, EnvironmentConfig(..), MachineConfig(..), Graph(..), Notification)

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
      environmentConfig <- prepareEnvironment configFile
      let [subgraphConfig] = splitGraphByMachine (graph environmentConfig)
      initializeMachines (machines environmentConfig) [subgraphConfig]
      evaluateProgram environmentConfig

-- Prepare the program graph and machine configs
prepareEnvironment :: FilePath -> IO EnvironmentConfig
prepareEnvironment configFile = do
  configData <- B.readFile configFile                        -- Read the configuration file as a ByteString
  case eitherDecode configData of                            -- Parse the configuration file into the `EnvironmentConfig` type
    Left err -> error $ "Failed to parse config: " ++ err
    Right environmentConfig -> return environmentConfig

-- Initialize machines with their respective subgraphs, latencies, and timeouts
initializeMachines :: [MachineConfig] -> [SubgraphConfig] -> IO ()
initializeMachines [machineConfig] [subgraphConfig] = mapM_ initializeMachine [subgraphConfig]
  where
    initializeMachine (machineID, subgraph) = do
      let config = case lookup machineID [(machineID mc, mc) | mc <- [machineConfig]] of
                     Just mc -> mc
                     Nothing -> error $ "Configuration not found for machine " ++ machineID
      putStrLn $ "Initializing machine " ++ machineID
      putStrLn $ "Subgraph: " ++ show subgraph
      putStrLn $ "Latency: " ++ show (latency config)
      putStrLn $ "Timeout: " ++ show (timeout config)

-- Evaluate the scheduled program and report outcome
evaluateProgram :: EnvironmentConfig -> IO ()
evaluateProgram environmentConfig = do
  putStrLn "Evaluating program..."           -- Notify the user about the evaluation process
  results <- evaluateProgram environmentConfig               -- Run the evaluation and capture the results
  putStrLn $ case results of                                 -- Print the results based on success or failure
    Just r -> "Evaluation succeeded: " ++ show r            -- Report success with results
    Nothing -> "Evaluation failed and Rescinded"            -- Report failure
