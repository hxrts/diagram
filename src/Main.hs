{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (SomeException, try)
import Data.List (isInfixOf)
import System.IO (writeFile)
import qualified Data.GraphViz as GraphViz
import Data.GraphViz.Printing (renderDot, toDot)
import Data.Text.Lazy (unpack)
import Data.Graph.Inductive.Graph (mkGraph, LNode, LEdge)
import Data.Graph.Inductive.PatriciaTree (Gr)

-- --------------------------------------
-- Definitions
-- --------------------------------------

-- Distributed actions to simulate holds and reservations
placeHold :: String -> String -> DistributedIO String
placeHold resource city = distributedAction $ "Placing hold on " ++ resource ++ " for " ++ city

releaseHold :: String -> String -> DistributedIO String
releaseHold resource city = distributedAction $ "Releasing hold on " ++ resource ++ " for " ++ city

bookResource :: String -> String -> DistributedIO String
bookResource resource city = distributedAction $ "Booking " ++ resource ++ " for " ++ city

-- Reusable Train-Hotel Booking Pattern
trainHotelBooking :: String -> [MachineID] -> [MachineID] -> EvalGraph String
trainHotelBooking city trainMachines hotelMachines =
  Atomic (Node trainMachines (placeHold "train" city) [
      Node hotelMachines (placeHold "hotel" city) [],
      Concurrent [
        Node trainMachines (bookResource "train" city) [],
        Node hotelMachines (bookResource "hotel" city) []
      ],
      Node trainMachines (releaseHold "train" city) [],
      Node hotelMachines (releaseHold "hotel" city) []
    ])

-- --------------------------------------
-- Graphing functions
-- --------------------------------------

-- Function to generate the control flow graph
generateControlFlowGraph :: EvalGraph a -> GraphViz.DotGraph String
generateControlFlowGraph graph =
  GraphViz.graphElemsToDot GraphViz.nonClusteredParams convertedNodes convertedEdges
  where
    (nodes, edges) = graphToNodesEdges graph
    convertedNodes = map (\(GraphViz.DotNode nodeId attrs) -> (nodeId, attrs)) nodes
    convertedEdges = map (\(f, t, label) -> (f, t, [GraphViz.toLabel label])) edges

-- Function to generate the execution path graph
generateExecutionPathGraph :: [Either SomeException a] -> EvalGraph a -> GraphViz.DotGraph String
generateExecutionPathGraph results graph = GraphViz.graphElemsToDot GraphViz.nonClusteredParams convertedNodes numberedEdges
  where
    (nodes, edges) = graphToNodesEdges graph
    executedNodes = getExecutedNodes results (map (\(GraphViz.DotNode n _) -> (n, n)) nodes)
    convertedNodes = map (\(GraphViz.DotNode nodeId attrs) -> (nodeId, attrs)) nodes
    numberedEdges = zipWith (\(f, t, _) i -> (f, t, [GraphViz.toLabel (show i)])) edges [1..]

-- Helper function to convert EvalGraph to nodes and edges
graphToNodesEdges :: EvalGraph a -> ([GraphViz.DotNode String], [(String, String, String)])
graphToNodesEdges = go ""
  where
    go parent (Node machines _ children) =
      let nodeId = unwords machines
          nodeLabel = unwords machines
          childResults = map (go nodeId) children
          childNodes = concatMap fst childResults
          childEdges = concatMap snd childResults
          edges = ([(parent, nodeId, "dependency") | not (null parent)])
      in (GraphViz.DotNode nodeId [GraphViz.toLabel nodeLabel] : childNodes, edges ++ childEdges)
    go parent (Atomic subgraph) =
      let (nodes, edges) = go parent subgraph
      in (nodes, map (\(f, t, _) -> (f, t, "atomic")) edges)
    go parent (Concurrent subgraphs) =
      let results = map (go parent) subgraphs
      in (concatMap fst results, concatMap snd results)

-- Helper function to get executed nodes based on results
getExecutedNodes :: [Either SomeException a] -> [(String, String)] -> [(String, String)]
getExecutedNodes results = filter (\(n, _) -> any (isNodeExecuted n) results)
  where
    isNodeExecuted node (Right _) = True
    isNodeExecuted _ _ = False

-- --------------------------------------
-- Main entry point
-- --------------------------------------

-- Main program logic
main :: IO ()
main = do
  -- Define faulty machines explicitly (simulate failures)
  let faultyMachines = ["MachineB"] -- Simulate failure of Berlin hotel machine

  -- Define the graph
  let berlinBooking = trainHotelBooking "Berlin" ["MachineA"] ["MachineB"]
  let amsterdamBooking = trainHotelBooking "Amsterdam" ["MachineA"] ["MachineC"]

  let graph = Node ["MachineA"] (distributedAction "Root Action") [
                  berlinBooking,   -- Attempt to book Berlin atomically
                  amsterdamBooking -- Fallback to Amsterdam
              ]

  -- Generate and save the control flow graph
  let controlFlowGraph = generateControlFlowGraph graph
  writeFile "out/control_flow_graph.dot" (unpack $ renderDot $ toDot controlFlowGraph)

  -- Evaluate the graph
  putStrLn "Starting the reservation process..."
  results <- evaluateGraph faultyMachines graph

  -- Generate and save the execution path graph
  let executionPathGraph = generateExecutionPathGraph results graph
  writeFile "out/execution_path_graph.dot" (unpack $ renderDot $ toDot executionPathGraph)

  -- Output the results
  putStrLn "\n==> Final Results:"
  print results

-- --------------------------------------
-- Types for failure recovery
-- --------------------------------------

type MachineID = String

-- List of faulty machines
type FaultyMachines = [MachineID]

-- DistributedIO represents a distributed computation that runs on an atomic subgraph of machines.
-- It takes a list of MachineIDs and a list of FaultyMachines and performs an IO action.
newtype DistributedIO a = DistributedIO {runDistributed :: [MachineID] -> FaultyMachines -> IO a}

-- Evaluation graph where each node in the graph is a subgraph of machines that can be run concurrently
data EvalGraph a
  = Node [MachineID] (DistributedIO a) [EvalGraph a]  -- A node representing a subgraph of machines and its dependencies
  | Atomic (EvalGraph a)                              -- An atomic subgraph
  | Concurrent [EvalGraph a]                          -- Concurrent subgraphs

-- Standalone deriving with constraint
deriving instance Show (DistributedIO a) => Show (EvalGraph a)

-- Dummy Show instance for DistributedIO
instance Show (DistributedIO a) where
  show _ = "<DistributedIO>"

-- --------------------------------------
-- Create distributed computations
-- --------------------------------------

-- Implicit distributed action that generates input based on a subgraph of machines
distributedAction :: String -> DistributedIO String
distributedAction action = DistributedIO $ \machineIDs faultyMachines -> do
  let inputs = map (\machineID -> action ++ " on " ++ machineID) machineIDs
  mapM_ (\input -> putStrLn $ "==> START: " ++ input) inputs
  results <- mapM (\machineID -> if machineID `elem` faultyMachines
                                  then do
                                    putStrLn $ "!!! FAILURE: Computation failed on Machine " ++ machineID
                                    return $ "Failure on Machine " ++ machineID
                                  else do
                                    let output = "Output from Machine " ++ machineID
                                    putStrLn $ "<== SUCCESS: " ++ output
                                    return output) machineIDs
  return $ unwords results

-- --------------------------------------
-- Graph evaluation functions
-- --------------------------------------

-- EvaluateGraph traverses the evaluation graph and executes distributed computations.
-- It handles dependencies and failures by evaluating each dependent node subgraph.
-- If a node fails, it logs the failure and continues with the remaining nodes.
-- The function returns a list of results, where each result is either a successful value or an exception.
evaluateGraph :: FaultyMachines -> EvalGraph a -> IO [Either SomeException a]
evaluateGraph faultyMachines (Node machines computation dependencies) = do
  putStrLn $ "==> Evaluating Node on Machines: " ++ unwords machines
  depResults <- traverse (evaluateGraph faultyMachines) dependencies
  result <- try $ runDistributed computation machines faultyMachines
  case result of
    Left (e :: SomeException) -> do
      putStrLn $ "!! FAILURE: Node on Machines " ++ unwords machines ++ " failed with: " ++ show e
      return (concat depResults ++ [Left e])
    Right val -> return (concat depResults ++ [Right val])

-- Evaluate an atomic subgraph
evaluateGraph faultyMachines (Atomic subgraph) = do
  putStrLn "==> START: Atomic Subgraph Evaluation"
  result <- try (evaluateGraph faultyMachines subgraph)
  case result of
    Left (e :: SomeException) -> do
      putStrLn $ "!! FAILURE: Atomic Subgraph FAILED: " ++ show e
      return [] -- Skip results due to failure
    Right res -> do
      putStrLn "==> SUCCESS: Atomic Subgraph Completed"
      return res

-- Evaluate concurrent subgraphs
evaluateGraph faultyMachines (Concurrent subgraphs) = do
  putStrLn "==> START: Concurrent Subgraph Evaluation"
  results <- mapConcurrently (evaluateGraph faultyMachines) subgraphs
  putStrLn "==> SUCCESS: Concurrent Subgraph Completed"
  return $ concat results
