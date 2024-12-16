module ProgramEvaluation (evaluateProgram, scheduleMachine, distributedAction) where

import Control.Concurrent.Async (mapConcurrently, race)
import Control.Concurrent (threadDelay)
import Data.Maybe (catMaybes, isNothing)
import Main (Graph(..), MachineConfig(..), MachineID, Notification)
import ProgramInstantiation (DistributedIO)

-- Helper function to convert latency from milliseconds to microseconds
convertLatency :: Int -> Int
convertLatency latencyMs = latencyMs * 1000

-- Create a DistributedIO action
distributedAction :: String -> String -> String -> MachineConfig -> DistributedIO Notification
distributedAction resource rollbackMessage notifyMessage config = DistributedIO
  { lock = \machineID -> putStrLn $ "Machine " ++ machineID ++ " locking: " ++ resource
  , commit = \machineID -> do
      putStrLn $ "Machine " ++ machineID ++ " committing: " ++ resource
      threadDelay (convertLatency (latency config)) -- Use machine's latency (convert to microseconds)
      return $ "Committed by " ++ machineID ++ ": " ++ resource
  , rollback = \machineID -> putStrLn $ "Machine " ++ machineID ++ " rolling back: " ++ rollbackMessage
  , free = \machineID message -> putStrLn $ "Machine " ++ machineID ++ " freeing: " ++ resource ++ " with message: " ++ message
  }

-- Evaluate the program
evaluateProgram :: Graph result -> [MachineConfig] -> IO (Maybe [result])
evaluateProgram graph machineConfigs = case graph of
  Node machine computation dependencies -> do
    let config = case lookup machine (map (\mc -> (machineID mc, mc)) machineConfigs) of
                   Just mc -> mc
                   Nothing -> error $ "Configuration not found for machine " ++ machine
    evaluateNode config computation dependencies machineConfigs
  Atomic subgraphs -> evaluateAtomic subgraphs machineConfigs
  Concurrent subgraphs -> evaluateConcurrent subgraphs machineConfigs

-- Evaluate a single Node
evaluateNode :: MachineConfig -> DistributedIO result -> [Graph result] -> [MachineConfig] -> IO (Maybe [result])
evaluateNode config computation dependencies machineConfigs = do
  lock computation (machineID config)
  depResults <- traverse (`evaluateProgram` machineConfigs) dependencies
  maybeRollback depResults $ do
    result <- race (threadDelay (convertLatency (timeout config))) (commit computation (machineID config)) -- Convert timeout to microseconds
    case result of
      Left _ -> do
        free computation (machineID config) "Timeout reached, freeing resource"
        rollbackAndNotify computation (machineID config) dependencies "Rollback due to timeout"
      Right value -> do
        free computation (machineID config) "Commit successful"
        return $ Just (concat (catMaybes depResults) ++ [value])

-- Evaluate an Atomic block
evaluateAtomic :: [Graph result] -> [MachineConfig] -> IO (Maybe [result])
evaluateAtomic subgraphs machineConfigs = do
  results <- traverse (`evaluateProgram` machineConfigs) subgraphs
  maybeRollback results $ return $ Just (concat (catMaybes results))

-- Evaluate Concurrent subgraphs
evaluateConcurrent :: [Graph result] -> [MachineConfig] -> IO (Maybe [result])
evaluateConcurrent subgraphs machineConfigs = do
  results <- mapConcurrently (`evaluateProgram` machineConfigs) subgraphs
  return $ fmap concat (sequence results)

-- Check dependency results and trigger rollback if any dependency fails
maybeRollback :: [Maybe [result]] -> IO (Maybe [result]) -> IO (Maybe [result])
maybeRollback depResults successAction
  | any isNothing depResults = return Nothing
  | otherwise = successAction

-- Scheduler for assigning machines dynamically
scheduleMachine :: [MachineID] -> Graph result -> Graph result
scheduleMachine [] _ = error "No available machines to schedule."
scheduleMachine availableMachines (Node _ computation dependencies) =
  Node (head availableMachines) computation (map (scheduleMachine availableMachines) dependencies)
scheduleMachine availableMachines (Atomic subgraphs) =
  Atomic (map (scheduleMachine availableMachines) subgraphs)
scheduleMachine availableMachines (Concurrent subgraphs) =
  Concurrent (map (scheduleMachine availableMachines) subgraphs)

-- Handle rollbacks and notifications
rollbackAndNotify :: DistributedIO result -> MachineID -> [Graph result] -> String -> IO (Maybe [result])
rollbackAndNotify computation machine dependencies message = do
  rollback computation machine
  rollbackDependencies dependencies
  free computation machine message
  return Nothing

-- Rollback all subgraphs
rollbackDependencies :: [Graph result] -> IO ()
rollbackDependencies = mapM_ rollbackGraph
  where
    rollbackGraph (Node machine computation dependencies) = do
      rollback computation machine
      rollbackDependencies dependencies
    rollbackGraph (Atomic subgraphs) = rollbackDependencies subgraphs
    rollbackGraph (Concurrent subgraphs) = rollbackDependencies subgraphs
