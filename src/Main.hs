module Main where

import Diagram.Evaluate (EvalGraph(..), evaluateGraph, distributedAction, DistributedIO, MachineID, FaultyMachines)
import Diagram.Graph (generateControlFlowGraph, generateEvaluationPathGraph)

import Control.Exception (SomeException)
import Data.GraphViz.Printing (renderDot, toDot)
import Data.Text.Lazy (unpack)
import System.IO (writeFile)

-- --------------------------------------
-- Define train + hotel atomic booking
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
  Atomic
    ( Node
        trainMachines
        (placeHold "train" city)
        [ Node hotelMachines (placeHold "hotel" city) []
        , Concurrent
            [ Node trainMachines (bookResource "train" city) []
            , Node hotelMachines (bookResource "hotel" city) []
            ]
        , Node trainMachines (releaseHold "train" city) []
        , Node hotelMachines (releaseHold "hotel" city) []
        ]
    )

-- --------------------------------------
-- Define machine layout
-- --------------------------------------

berlinBooking :: EvalGraph String
berlinBooking = trainHotelBooking "Berlin" ["MachineA"] ["MachineB"]

amsterdamBooking :: EvalGraph String
amsterdamBooking = trainHotelBooking "Amsterdam" ["MachineA"] ["MachineC"]

-- --------------------------------------
-- Define booking preference control flow
-- --------------------------------------

graph :: EvalGraph String
graph =
  Node
    ["MachineA"]
    (distributedAction "Root Action")
    [ Conditional (evaluateSuccess faultyMachines berlinBooking) berlinBooking amsterdamBooking ] -- Attempt Berlin booking and fallback to Amsterdam atomic booking if Berlin fails

-- helper function to evaluate the success of a subgraph
evaluateSuccess :: FaultyMachines -> EvalGraph a -> IO Bool
evaluateSuccess faultyMachines subgraph = do
  results <- evaluateGraph faultyMachines subgraph
  return $ all isRight results
  where
    isRight (Right _) = True
    isRight _ = False

-- --------------------------------------
-- Define machine runtime behavior
-- --------------------------------------

faultyMachines :: [MachineID]
faultyMachines = ["MachineB"] -- Simulate failure of Berlin hotel machine

-- --------------------------------------
-- Main entry point
-- --------------------------------------

main :: IO ()
main = do
  -- Generate and save the control flow graph
  putStrLn "\nBuilding control flow graph..."
  controlFlowGraph <- generateControlFlowGraph graph
  writeFile "out/control_flow_graph.dot" (unpack $ renderDot $ toDot controlFlowGraph)

  -- Evaluate the graph including simulated faults
  putStrLn "\nEvaluating distributed computation..."
  evaluationResults <- evaluateGraph faultyMachines graph

  -- Generate and save the evaluation path graph
  evaluationPathGraph <- generateEvaluationPathGraph evaluationResults graph
  writeFile "out/evaluation_path_graph.dot" (unpack $ renderDot $ toDot evaluationPathGraph)

  -- Output the evaluation results
  putStrLn "\nEvaluation results:"
  print evaluationResults
