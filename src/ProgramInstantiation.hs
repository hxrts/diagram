{-# LANGUAGE DeriveGeneric #-}

module ProgramInstantiation (Config(..), MachineConfig(..), ControlFlowGraph(..), ControlFlowGraphConfig(..), convertGraph, splitGraphByMachine) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import ProgramEvaluation (distributedAction)

-- Type alias for machine identifiers
type MachineID = String

-- Configuration for each machine
data MachineConfig = MachineConfig
  { machineID :: MachineID
  , latency   :: Int
  } deriving (Show, Generic, FromJSON, ToJSON)

-- Main configuration structure of the program
data Config = Config
  { machines :: [MachineConfig]            -- List of available machines with their latencies
  , dag      :: ControlFlowGraphConfig     -- Definition of the DAG
  , timeoutThreshold :: Int                -- Global timeout threshold for computation
  } deriving (Show, Generic, FromJSON, ToJSON)

-- ControlFlowGraph is a DAG representing the program structure
data ControlFlowGraph a
  = Node MachineID (DistributedIO a) [ControlFlowGraph a] -- A computation node with dependencies
  | Atomic [ControlFlowGraph a]                           -- A subgraph that must execute atomically
  | Concurrent [ControlFlowGraph a]                       -- Independent subgraphs that can execute concurrently
  deriving (Show)

-- Representation of the DAG (Directed Acyclic Graph) in the configuration file
data ControlFlowGraphConfig
  = EvalNode MachineID String String String [ControlFlowGraphConfig]  -- A single computation node
  | EvalAtomic [ControlFlowGraphConfig]                               -- Atomic block
  | EvalConcurrent [ControlFlowGraphConfig]                           -- Concurrent block
  deriving (Show, Generic, FromJSON, ToJSON)

-- Converts a DAG configuration into a ControlFlowGraph that can be evaluated
convertGraph :: ControlFlowGraphConfig -> ControlFlowGraph String
convertGraph (EvalNode machine resource rollbackMessage notifyMessage dependencies) =
  Node machine (distributedAction resource rollbackMessage notifyMessage) (map convertGraph dependencies)
convertGraph (EvalAtomic subgraphs) = Atomic (map convertGraph subgraphs)
convertGraph (EvalConcurrent subgraphs) = Concurrent (map convertGraph subgraphs)

-- Split the ControlFlowGraph into subgraphs for each machine
splitGraphByMachine :: ControlFlowGraph a -> [(MachineID, ControlFlowGraph a)]
splitGraphByMachine graph = case graph of
  Node machine computation dependencies ->
    [(machine, Node machine computation dependencies)] ++ concatMap splitGraphByMachine dependencies
  Atomic subgraphs -> concatMap splitGraphByMachine subgraphs
  Concurrent subgraphs -> concatMap splitGraphByMachine subgraphs
