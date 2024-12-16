-- Test suite for the distributed computation framework

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as B
import ProgramInstantiation (Config(..), MachineConfig(..), Graph(..), convertGraph, splitGraphByMachine)
import ProgramEvaluation (evaluateProgram, scheduleMachine)
import System.IO (hFlush, stdout)
import System.IO.Silently (capture)

-- Load a configuration file and evaluate it with a specific timeout
loadAndEvaluateConfig :: FilePath -> IO (Maybe [String], String)
loadAndEvaluateConfig configFilePath = do
  -- Read the configuration file
  configData <- B.readFile configFilePath
  -- Parse the configuration file into the `Config` type
  case eitherDecode configData of
    Left err -> error $ "Failed to parse config: " ++ err
    Right (Config [MachineConfig] graphConfig) -> do
      -- Convert the DAG configuration into a graph representation
      let controlFlowGraph = convertGraph graphConfig
      -- Schedule the graph computation across the available machines
      let graphConfig = scheduleMachine (map machineID [MachineConfig]) controlFlowGraph
      -- Split the graph by machine
      let [SubgraphConfig] = splitGraphByMachine graphConfig
      -- Capture the output of the evaluation
      output <- capture $ do
        hFlush stdout
        -- Print machine configurations
        mapM_ (printMachineConfig [MachineConfig]) [SubgraphConfig]
        result <- evaluateProgram graphConfig [MachineConfig]
        hFlush stdout
        return result
      return output

-- Print machine configuration
printMachineConfig :: [MachineConfig] -> (MachineID, Graph String) -> IO ()
printMachineConfig [MachineConfig] (machineID, subgraph) = do
  let config = case lookup machineID (map (\mc -> (machineID mc, mc)) [MachineConfig]) of
                 Just mc -> mc
                 Nothing -> error $ "Configuration not found for machine " ++ machineID
  putStrLn $ "MachineID: " ++ machineID
  putStrLn $ "Latency: " ++ show (latency config)
  putStrLn $ "Timeout: " ++ show (timeout config)
  putStrLn $ "Subgraph: " ++ show subgraph

-- Test suite
main :: IO ()
main = hspec $ do
  describe "Begin program evaluation" $ do
    -- Test case for retrying Train and Hotel Booking after initial failure
    it "retries Train and Hotel Booking after initial failure" $ do
      (result, output) <- loadAndEvaluateConfig "test/TrainHotelRetry.json"
      putStrLn output
      result `shouldBe` Just ["Committed by MachineA: Train Berlin to Paris Hold",
                              "Committed by MachineC: Paris Hotel Reserve"]
