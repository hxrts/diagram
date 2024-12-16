module ProgramEvaluation (evaluateProgram, scheduleMachine, distributedAction) where

import Control.Concurrent.Async (mapConcurrently, race)
import Control.Concurrent (threadDelay)
import Data.Maybe (catMaybes, isNothing)
import ProgramInstantiation (ControlFlowGraph(..))

-- DistributedIO represents the lifecycle hooks for a distributed computation
data DistributedIO a = DistributedIO
  { lock     :: MachineID -> IO ()           -- Lock resources before performing the action
  , commit   :: MachineID -> IO a            -- Commit resources (perform the main action)
  , rollback :: MachineID -> IO ()           -- Rollback changes if needed
  , free     :: MachineID -> String -> IO () -- Free resources and notify machines about the outcome
  }

-- Create a DistributedIO action
distributedAction :: String -> String -> String -> DistributedIO String
distributedAction resource rollbackMessage notifyMessage = DistributedIO
  { lock = \machineID -> putStrLn $ "Machine " ++ machineID ++ " locking: " ++ resource
  , commit = \machineID -> do
      putStrLn $ "Machine " ++ machineID ++ " committing: " ++ resource
      threadDelay 500000 -- Simulate delay (500ms)
      return $ "Committed by " ++ machineID ++ ": " ++ resource
  , rollback = \machineID -> putStrLn $ "Machine " ++ machineID ++ " rolling back: " ++ rollbackMessage
  , free = \machineID message -> putStrLn $ "Machine " ++ machineID ++ " freeing: " ++ resource ++ " with message: " ++ message
  }

-- Evaluate the program with a per-test timeout
evaluateProgram :: ControlFlowGraph a -> Int -> IO (Maybe [a])
evaluateProgram graph timeout = case graph of
  Node machine computation dependencies -> evaluateNode machine computation dependencies timeout
  Atomic subgraphs -> evaluateAtomic subgraphs timeout
  Concurrent subgraphs -> evaluateConcurrent subgraphs timeout

-- Evaluate a single Node
evaluateNode :: MachineID -> DistributedIO a -> [ControlFlowGraph a] -> Int -> IO (Maybe [a])
evaluateNode machine computation dependencies timeout = do
  lock computation machine
  depResults <- traverse (`evaluateProgram` timeout) dependencies
  maybeRollback depResults $ do
    result <- race (threadDelay timeout) (commit computation machine)
    case result of
      Left _ -> rollbackAndNotify computation machine dependencies "Rollback due to timeout"
      Right value -> do
        free computation machine "Commit successful"
        return $ Just (concat (catMaybes depResults) ++ [value])

-- Evaluate an Atomic block
evaluateAtomic :: [ControlFlowGraph a] -> Int -> IO (Maybe [a])
evaluateAtomic subgraphs timeout = do
  results <- traverse (`evaluateProgram` timeout) subgraphs
  maybeRollback results $ return $ Just (concat (catMaybes results))

-- Evaluate Concurrent subgraphs
evaluateConcurrent :: [ControlFlowGraph a] -> Int -> IO (Maybe [a])
evaluateConcurrent subgraphs timeout = do
  results <- mapConcurrently (`evaluateProgram` timeout) subgraphs
  return $ fmap concat (sequence results)

-- Check dependency results and trigger rollback if any dependency fails
maybeRollback :: [Maybe [a]] -> IO (Maybe [a]) -> IO (Maybe [a])
maybeRollback depResults successAction
  | any isNothing depResults = return Nothing
  | otherwise = successAction

-- Scheduler for assigning machines dynamically
scheduleMachine :: [MachineID] -> ControlFlowGraph a -> ControlFlowGraph a
scheduleMachine [] _ = error "No available machines to schedule."
scheduleMachine availableMachines (Node _ computation dependencies) =
  Node (head availableMachines) computation (map (scheduleMachine availableMachines) dependencies)
scheduleMachine availableMachines (Atomic subgraphs) =
  Atomic (map (scheduleMachine availableMachines) subgraphs)
scheduleMachine availableMachines (Concurrent subgraphs) =
  Concurrent (map (scheduleMachine availableMachines) subgraphs)

-- Handle rollbacks and notifications
rollbackAndNotify :: DistributedIO a -> MachineID -> [ControlFlowGraph a] -> String -> IO (Maybe [a])
rollbackAndNotify computation machine dependencies message = do
  rollback computation machine
  rollbackDependencies dependencies
  free computation machine message
  return Nothing

-- Rollback all subgraphs
rollbackDependencies :: [ControlFlowGraph a] -> IO ()
rollbackDependencies = mapM_ rollbackGraph
  where
    rollbackGraph (Node machine computation dependencies) = do
      rollback computation machine
      rollbackDependencies dependencies
    rollbackGraph (Atomic subgraphs) = rollbackDependencies subgraphs
    rollbackGraph (Concurrent subgraphs) = rollbackDependencies subgraphs
