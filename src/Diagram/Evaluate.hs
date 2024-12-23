{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MonoLocalBinds #-}

module Diagram.Evaluate where

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (SomeException, try)

-- --------------------------------------
-- Types
-- --------------------------------------

type MachineID = String
type FaultyMachines = [MachineID]

newtype DistributedIO a = DistributedIO {runDistributed :: [MachineID] -> FaultyMachines -> IO a}

data EvalGraph a
  = Node [MachineID] (DistributedIO a) [EvalGraph a]
  | Atomic (EvalGraph a)
  | Concurrent [EvalGraph a]
  | Conditional (IO Bool) (EvalGraph a) (EvalGraph a)

instance Show (DistributedIO a) where
  show _ = "<DistributedIO>"

instance Show (EvalGraph a) where
  show (Node machines _ children) = "Node " ++ show machines ++ " " ++ show children
  show (Atomic subgraph) = "Atomic " ++ show subgraph
  show (Concurrent subgraphs) = "Concurrent " ++ show subgraphs
  show (Conditional _ trueBranch falseBranch) = "Conditional <condition> " ++ show trueBranch ++ " " ++ show falseBranch

-- --------------------------------------
-- Distributed action
-- --------------------------------------

-- Define a distributed action
distributedAction :: String -> DistributedIO String
distributedAction action = DistributedIO $ \machineIDs faultyMachines -> do
  let inputs = map (\machineID -> action ++ " on " ++ machineID) machineIDs
  results <-
    mapM
      ( \machineID ->
          if machineID `elem` faultyMachines
            then do
              let output = action ++ " on " ++ machineID
              putStrLn $ "  ! FAILURE:    " ++ output
              return output
            else do
              let output = action ++ " on " ++ machineID
              putStrLn $ "  $ SUCCESS:    " ++ output
              return output
      )
      machineIDs
  return $ unwords results

-- --------------------------------------
-- Graph evaluation
-- --------------------------------------

evaluateGraph :: FaultyMachines -> EvalGraph a -> IO [Either SomeException a]
evaluateGraph faultyMachines (Node machines computation dependencies) = do
  let machineList = unwords machines
  depResults <- traverse (evaluateGraph faultyMachines) dependencies
  result <- try $ runDistributed computation machines faultyMachines
  case result of
    Left (exception :: SomeException) -> do
      putStrLn $ "  ! FAILURE:    Node on machines " ++ machineList ++ " failed with " ++ show exception
      return (concat depResults ++ [Left exception])
    Right val -> return (concat depResults ++ [Right val])

evaluateGraph faultyMachines (Atomic subgraph) = do
  putStrLn "--> START:      Atomic subgraph evaluation"
  result <- try (evaluateGraph faultyMachines subgraph)
  case result of
    Left (exception :: SomeException) -> do
      putStrLn $ "  ! FAILURE:    Atomic subgraph failed with " ++ show exception
      return []
    Right res -> do
      putStrLn "<-- SUCESS:     Atomic subgraph evaluation"
      return res

evaluateGraph faultyMachines (Concurrent subgraphs) = do
  putStrLn "==> START:      Concurrent subgraph evaluation"
  results <- mapConcurrently (evaluateGraph faultyMachines) subgraphs
  putStrLn "<== END:        Concurrent subgraph evaluation"
  return $ concat results

evaluateGraph faultyMachines (Conditional condition trueBranch falseBranch) = do
  cond <- condition
  if cond
    then do
      putStrLn "  $ COND MET:   Evaluating true branch"
      evaluateGraph faultyMachines trueBranch
    else do
      putStrLn "  $ COND NOT MET: Evaluating false branch"
      evaluateGraph faultyMachines falseBranch