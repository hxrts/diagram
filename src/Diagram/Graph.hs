module Diagram.Graph where

import Diagram.Evaluate (EvalGraph(..), DistributedIO(..), MachineID, FaultyMachines)

import qualified Data.GraphViz as GraphViz
import qualified Data.GraphViz.Attributes.Complete as GraphViz
import Control.Exception (SomeException)
import Control.Monad (foldM)

-- --------------------------------------
-- Graph utilities
-- --------------------------------------

-- Convert a graph to nodes and edges
graphToNodesEdges :: EvalGraph String -> IO ([GraphViz.DotNode String], [(String, String, String)])
graphToNodesEdges = traverseGraph 0 Nothing
  where
    -- Traverse the graph and process each node
    traverseGraph :: Int -> Maybe String -> EvalGraph String -> IO ([GraphViz.DotNode String], [(String, String, String)])
    traverseGraph counter parent graph = case graph of
      Node machines (DistributedIO action) children -> do
        result <- action machines []
        let nodeId = "Node_" ++ show counter
            nodeLabel = result ++ " on " ++ unwords machines
        processNode counter parent nodeId nodeLabel children (Node machines (DistributedIO action) children)
      Atomic subgraph -> do
        let nodeId = "Atomic_" ++ show counter
            nodeLabel = "Atomic Block"
        processNode counter parent nodeId nodeLabel [subgraph] (Atomic subgraph)
      Concurrent subgraphs -> do
        let nodeId = "Concurrent_" ++ show counter
            nodeLabel = "Concurrent Block"
        processNode counter parent nodeId nodeLabel subgraphs (Concurrent subgraphs)
      Conditional _ trueBranch falseBranch -> do
        let nodeId = "Conditional_" ++ show counter
            nodeLabel = "Conditional Block"
        processNode counter parent nodeId nodeLabel [trueBranch, falseBranch] (Conditional (return True) trueBranch falseBranch)

    -- Process a node with its children
    processNode :: Int -> Maybe String -> String -> String -> [EvalGraph String] -> EvalGraph String -> IO ([GraphViz.DotNode String], [(String, String, String)])
    processNode counter maybeParentId nodeId nodeLabel children graph = do
      -- Create the current node
      let currentNode = GraphViz.DotNode nodeId [GraphViz.toLabel nodeLabel]
          -- Create the edge from the parent node to the current node, if a parent exists
          parentEdge = maybe [] (\parentId -> [(parentId, nodeId, edgeLabel graph)]) maybeParentId
      -- Traverse the children of the current node
      (childNodes, childEdges) <- foldM (\(accNodes, accEdges) child -> do
        (childNodes, childEdges) <- traverseGraph (counter + 1) (Just nodeId) child
        return (accNodes ++ childNodes, accEdges ++ childEdges)) ([], []) children
      -- Return the current node and its edges, along with the child nodes and edges
      return (currentNode : childNodes, parentEdge ++ childEdges)

    -- Determine the edge label based on the graph type
    edgeLabel :: EvalGraph a -> String
    edgeLabel (Node {}) = "Depends"
    edgeLabel (Atomic _) = "Atomic"
    edgeLabel (Concurrent _) = "Concurrent"
    edgeLabel (Conditional {}) = "Conditional"

-- --------------------------------------
-- Graph generation
-- --------------------------------------

-- Generate a control flow graph
generateControlFlowGraph :: EvalGraph String -> IO (GraphViz.DotGraph String)
generateControlFlowGraph graph = do
  (nodes, edges) <- graphToNodesEdges graph
  let convertedNodes = map (\(GraphViz.DotNode nodeId attrs) -> (nodeId, attrs ++ [GraphViz.toLabel $ "Machines: " ++ nodeId])) nodes
      convertedEdges = map (\(from, to, label) -> (from, to, [GraphViz.toLabel label])) edges
  return $ GraphViz.graphElemsToDot GraphViz.nonClusteredParams convertedNodes convertedEdges

-- Generate an evaluation path graph
generateEvaluationPathGraph :: [Either SomeException String] -> EvalGraph String -> IO (GraphViz.DotGraph String)
generateEvaluationPathGraph results graph = do
  let actionResults = zip [0..] results
  -- Process each result to create nodes and edges
  (nodes, edges) <- foldM processResult ([], []) actionResults
  let convertedNodes = map (\(GraphViz.DotNode nodeId attrs) -> (nodeId, attrs)) nodes
      convertedEdges = map (\(from, to, label) -> (from, to, [GraphViz.toLabel $ actionLabel label])) edges
  return $ GraphViz.graphElemsToDot GraphViz.nonClusteredParams convertedNodes convertedEdges
  where
    -- Process each result to create a node and its edge
    processResult :: ([GraphViz.DotNode String], [(String, String, Either SomeException String)]) -> (Int, Either SomeException String) -> IO ([GraphViz.DotNode String], [(String, String, Either SomeException String)])
    processResult (nodes, edges) (index, result) = do
      let nodeId = "State_" ++ show index
          nodeLabel = case result of
            Left _ -> "Failed Action"
            Right action -> "State: " ++ action
          currentNode = GraphViz.DotNode nodeId [GraphViz.toLabel nodeLabel]
          -- Create an edge from the previous state to the current state
          parentEdge = ([("State_" ++ show (index - 1), nodeId, result) | index /= 0])
      return (nodes ++ [currentNode], edges ++ parentEdge)

    -- Generate a label for the action
    actionLabel :: Either SomeException String -> String
    actionLabel (Left _) = "Failed Action"
    actionLabel (Right action) = action
