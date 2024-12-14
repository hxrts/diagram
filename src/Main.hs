module Main where

import DistributedIO.Core

main :: IO ()
main = do
  -- Define distributed computations
  let action1 = distributedAction "Action 1 Input"
  let action2 = distributedAction "Action 2 Input"
  let action3 = distributedAction "Action 3 Input"
  let action4 = distributedAction "Action 4 Input"
  let rootAction = distributedAction "Root Input"

  -- Define the DAG
  let graph =
        Node "MachineA" rootAction
          [ Atomic (Concurrent
              [ Node "MachineB" action1 []
              , Node "MachineC" action2 []
              ])
          , Node "MachineD" action3 [Node "MachineE" action4 []]
          ]

  -- Evaluate the graph
  results <- evaluateGraph graph
  print results
