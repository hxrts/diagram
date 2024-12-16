module ProgramEvaluation (evaluateProgram, scheduleMachine) where

import Control.Concurrent.Async (mapConcurrently, race)
import Control.Concurrent (threadDelay)
import Data.Maybe (catMaybes, isNothing)
import ProgramInstantiation (EnvironmentConfig(..), MachineConfig(..), Graph(..), DistributedIO)

-- Evaluate the program
evaluateProgram :: EnvironmentConfig -> IO (Maybe [result])
evaluateProgram (EnvironmentConfig machineConfigs graphConfig) = do
  let graph = convertGraph graphConfig machineConfigs
  evaluateGraph graph machineConfigs

-- Evaluate the graph
evaluateGraph :: Graph result -> [MachineConfig] -> IO (Maybe [result])
evaluateGraph graph machineConfigs = case graph of
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
  depResults <- traverse (`evaluateGraph` machineConfigs) dependencies
  maybeRollback depResults $ do
    result <- race (threadDelay (timeout config)) (commit computation (machineID config))
    case result of
      Left _ -> do
        free computation (machineID config) "Timeout reached, freeing"
        rescindAndNotify computation (machineID config) dependencies "Rescinding"
      Right value -> do
        free computation (machineID config) "Committing"
        return $ Just (concat (catMaybes depResults) ++ [value])

-- Evaluate an Atomic block
evaluateAtomic :: [Graph result] -> [MachineConfig] -> IO (Maybe [result])
evaluateAtomic subgraphs machineConfigs = do
  results <- traverse (`evaluateGraph` machineConfigs) subgraphs
  maybeRollback results $ return $ Just (concat (catMaybes results))

-- Evaluate Concurrent subgraphs
evaluateConcurrent :: [Graph result] -> [MachineConfig] -> IO (Maybe [result])
evaluateConcurrent subgraphs machineConfigs = do
  results <- mapConcurrently (`evaluateGraph` machineConfigs) subgraphs
  return $ fmap concat (sequence results)

-- Check dependency results and trigger rollback if any dependency fails
maybeRollback :: [Maybe [result]] -> IO (Maybe [result]) -> IO (Maybe [result])
maybeRollback depResults successAction
  | any isNothing depResults = return Nothing
  | otherwise = successAction

-- Scheduler for assigning machines dynamically
scheduleMachine :: [MachineID] -> Graph result -> Graph result
scheduleMachine [] _ = error "No available machines to schedule"
scheduleMachine availableMachines (Node _ computation dependencies) =
  Node (head availableMachines) computation (map (scheduleMachine availableMachines) dependencies)
scheduleMachine availableMachines (Atomic subgraphs) =
  Atomic (map (scheduleMachine availableMachines) subgraphs)
scheduleMachine availableMachines (Concurrent subgraphs) =
  Concurrent (map (scheduleMachine availableMachines) subgraphs)

-- Handle rescinds and notifications
rescindAndNotify :: DistributedIO result -> MachineID -> [Graph result] -> String -> IO (Maybe [result])
rescindAndNotify computation machine dependencies message = do
  rescind computation machine
  rescindDependencies dependencies
  free computation machine message
  return Nothing

-- Rescind all subgraphs
rescindDependencies :: [Graph result] -> IO ()
rescindDependencies = mapM_ rescindGraph
  where
    rescindGraph (Node machine computation dependencies) = do
      rescind computation machine
      rescindDependencies dependencies
    rescindGraph (Atomic subgraphs) = rescindDependencies subgraphs
    rescindGraph (Concurrent subgraphs) = rescindDependencies subgraphs
