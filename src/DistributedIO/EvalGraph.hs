module DistributedIO.EvalGraph
  ( EvalGraph(..),
    evaluateGraph,
  ) where

import DistributedIO.Core (DistributedIO(..))
import Control.Concurrent.Async (mapConcurrently)

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
  return [results]

evaluateGraph (Concurrent subgraphs) = do
  -- Evaluate all subgraphs concurrently
  concat <$> mapConcurrently evaluateGraph subgraphs
