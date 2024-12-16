-- Test suite for the distributed computation framework

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.Hspec
import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as B
import ProgramInstantiation (Config(..), MachineConfig(..), ControlFlowGraph(..), convertGraph, splitGraphByMachine)
import ProgramEvaluation (evaluateProgram, scheduleMachine)

-- Load a configuration file and evaluate it with a specific timeout
loadAndEvaluateConfig :: FilePath -> IO (Maybe [String])
loadAndEvaluateConfig configFilePath = do
  -- Read the configuration file
  configData <- B.readFile configFilePath
  -- Parse the configuration file into the `Config` type
  case eitherDecode configData of
    Left err -> error $ "Failed to parse config: " ++ err
    Right (Config machineConfigs dagConfig) -> do
      -- Convert the DAG configuration into a graph representation
      let controlFlowGraph = convertGraph dagConfig
      -- Schedule the graph computation across the available machines
      let scheduledProgram = scheduleMachine (map machineID machineConfigs) controlFlowGraph
      -- Evaluate the scheduled program with the provided timeout
      evaluateProgram scheduledProgram machineConfigs

-- Test suite
main :: IO ()
main = hspec $ do
  describe "test distributed computation" $ do
    -- Test case for retrying Train and Hotel Booking after initial failure
    it "retries Train and Hotel Booking after initial failure" $ do
      result <- loadAndEvaluateConfig "test/TrainHotelRetry.json"
      result `shouldBe` Just ["Committed by MachineA: Train Berlin to Paris Hold",
                              "Committed by MachineC: Paris Hotel Reserve"]
