module DistributedIO.Core where

import Control.Concurrent.Async (mapConcurrently)


-- --------------------------------------
-- create distributed compute environment
-- --------------------------------------

-- A type to represent a machine identifier
type MachineID = String

-- A type for distributed IO computations
data DistributedIO a = DistributedIO
  { runDistributed :: MachineID -> IO a -- Execute the computation on a given machine
  }

-- Show instance for DistributedIO
instance Show (DistributedIO a) where
  show _ = "<DistributedIO>"

-- Simulate running a computation on a specific machine
simulateMachine :: MachineID -> String -> IO String
simulateMachine machineID input = do
  putStrLn $ "Machine " ++ machineID ++ " processing: " ++ input
  return $ "Output from " ++ machineID ++ " for input: " ++ input

-- Create a distributed computation
distributedAction :: String -> DistributedIO String
distributedAction input = DistributedIO $ \machineID -> simulateMachine machineID input

-- ------------------------------
-- create graph evaluation system
-- ------------------------------

-- The evaluation graph
data EvalGraph a
  = Node MachineID (DistributedIO a) [EvalGraph a] -- A node with a machine, computation, and dependencies
  | Atomic (EvalGraph a)                           -- An atomic subgraph
  | Concurrent [EvalGraph a]                       -- Subgraphs evaluated concurrently
  deriving Show

-- Evaluation Function
evaluateGraph :: EvalGraph a -> IO [a]
evaluateGraph (Node machine computation dependencies) = do
  -- Evaluate dependencies first
  depResults <- traverse evaluateGraph dependencies
  -- Execute the computation on the specified machine
  result <- runDistributed computation machine
  return (concat depResults ++ [result])

evaluateGraph (Atomic subgraph) = do
  -- Evaluate the entire subgraph atomically
  results <- evaluateGraph subgraph
  return results

evaluateGraph (Concurrent subgraphs) = do
  -- Evaluate all subgraphs concurrently
  concat <$> mapConcurrently evaluateGraph subgraphs
