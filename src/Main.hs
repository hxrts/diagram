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
trainHotelBooking :: String -> MachineID -> MachineID -> EvalGraph String
trainHotelBooking city trainMachine hotelMachine =
  Atomic (Node trainMachine (placeHold "train" city) [
      Node hotelMachine (placeHold "hotel" city) [],
      Concurrent [
        Node trainMachine (bookResource "train" city) [],
        Node hotelMachine (bookResource "hotel" city) []
      ],
      Node trainMachine (releaseHold "train" city) [],
      Node hotelMachine (releaseHold "hotel" city) []
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
    convertedEdges = map (\(f, t) -> (f, t, ())) edges

-- Function to generate the execution path graph
generateExecutionPathGraph :: [Either SomeException a] -> EvalGraph a -> GraphViz.DotGraph String
generateExecutionPathGraph results graph =
  GraphViz.graphElemsToDot GraphViz.nonClusteredParams convertedNodes convertedEdges
  where
    (nodes, edges) = graphToNodesEdges graph
    executedNodes = getExecutedNodes results (map (\(GraphViz.DotNode n _) -> (n, n)) nodes)
    convertedNodes = map (\(GraphViz.DotNode nodeId attrs) -> (nodeId, attrs)) nodes
    convertedEdges = map (\(f, t) -> (f, t, ())) edges

-- Helper function to convert EvalGraph to nodes and edges
graphToNodesEdges :: EvalGraph a -> ([GraphViz.DotNode String], [(String, String)])
graphToNodesEdges = go ""
  where
    go parent (Node machine _ children) =
      let nodeId = machine
          nodeLabel = machine
          childResults = map (go nodeId) children
          childNodes = concatMap fst childResults
          childEdges = concatMap snd childResults
          edges = if null parent then [] else [(parent, nodeId)]
      in ((GraphViz.DotNode nodeId [GraphViz.toLabel nodeLabel]) : childNodes, edges ++ childEdges)
    go parent (Atomic subgraph) = go parent subgraph
    go parent (Concurrent subgraphs) =
      let results = map (go parent) subgraphs
      in (concatMap fst results, concatMap snd results)

-- Helper function to get executed nodes based on results
getExecutedNodes :: [Either SomeException a] -> [(String, String)] -> [(String, String)]
getExecutedNodes results nodes = filter (\(n, _) -> any (isNodeExecuted n) results) nodes
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
  let faultyMachines = ["MachineB"] -- Simulate failure on Berlin hotel

  -- Define the graph
  let berlinBooking = trainHotelBooking "Berlin" "MachineA" "MachineB"
  let amsterdamBooking = trainHotelBooking "Amsterdam" "MachineA" "MachineC"

  let graph = Node "MachineA" (distributedAction "Root Action") [
                  berlinBooking,   -- Attempt to book Berlin atomically
                  amsterdamBooking -- Fallback to Amsterdam
              ]

  -- Generate and save the control flow graph
  let controlFlowGraph = generateControlFlowGraph graph
  writeFile "control_flow_graph.dot" (unpack $ renderDot $ toDot controlFlowGraph)

  -- Evaluate the graph
  putStrLn "Starting the reservation process..."
  results <- evaluateGraph faultyMachines graph

  -- Generate and save the execution path graph
  let executionPathGraph = generateExecutionPathGraph results graph
  writeFile "execution_path_graph.dot" (unpack $ renderDot $ toDot executionPathGraph)

  -- Output the results
  putStrLn "\n==> Final Results:"
  print results

-- --------------------------------------
-- Types for failure recovery
-- --------------------------------------

type MachineID = String

-- Faulty machines list
type FaultyMachines = [MachineID]

-- Distributed IO type
newtype DistributedIO a = DistributedIO {runDistributed :: MachineID -> FaultyMachines -> IO a}

data EvalGraph a
  = Node MachineID (DistributedIO a) [EvalGraph a]
  | Atomic (EvalGraph a)
  | Concurrent [EvalGraph a]

-- Standalone deriving with constraint
deriving instance Show (DistributedIO a) => Show (EvalGraph a)

-- Dummy Show instance for DistributedIO
instance Show (DistributedIO a) where
  show _ = "<DistributedIO>"

-- --------------------------------------
-- Create distributed computations
-- --------------------------------------

-- Implicit distributed action that generates input based on the machine
distributedAction :: String -> DistributedIO String
distributedAction action = DistributedIO $ \machineID faultyMachines -> do
  let input = action ++ " on " ++ machineID
  putStrLn $ "==> START: " ++ input
  -- Check if the machine is faulty
  if machineID `elem` faultyMachines
      then do
          putStrLn $ "!!! FAILURE: Computation failed on Machine " ++ machineID
          return $ "Failure on Machine " ++ machineID
      else do
          let output = "Output from Machine " ++ machineID
          putStrLn $ "<== SUCCESS: " ++ output
          return output

-- --------------------------------------
-- Graph evaluation functions
-- --------------------------------------

-- Evaluate graph with explicit faulty machines
evaluateGraph :: FaultyMachines -> EvalGraph a -> IO [Either SomeException a]
evaluateGraph faultyMachines (Node machine computation dependencies) = do
  putStrLn $ "==> Evaluating Node on Machine: " ++ machine
  depResults <- traverse (evaluateGraph faultyMachines) dependencies
  result <- try $ runDistributed computation machine faultyMachines
  case result of
    Left (e :: SomeException) -> do
      putStrLn $ "!! FAILURE: Node on Machine " ++ machine ++ " failed with: " ++ show e
      return (concat depResults ++ [Left e])
    Right val -> return (concat depResults ++ [Right val])

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

evaluateGraph faultyMachines (Concurrent subgraphs) = do
  putStrLn "==> START: Concurrent Subgraph Evaluation"
  results <- mapConcurrently (evaluateGraph faultyMachines) subgraphs
  putStrLn "==> SUCCESS: Concurrent Subgraph Completed"
  return $ concat results
