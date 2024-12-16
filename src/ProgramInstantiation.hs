{-# LANGUAGE DeriveGeneric #-}

module ProgramInstantiation (EnvironmentConfig(..), MachineConfig(..), Graph(..), GraphConfig(..), Notification, DistributedIO(..), convertGraph, splitGraphByMachine) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import ProgramEvaluation (distributedAction)

type MachineID = String
type Notification = String

-- All machines and the DAG connecting them
data EnvironmentConfig = EnvironmentConfig
  { machines :: [MachineConfig]            -- List of available machines with their latencies and timeouts
  , graph    :: GraphConfig                -- Definition of the DAG
  } deriving (Show, Generic, FromJSON, ToJSON)

-- Configuration for each machine
data MachineConfig = MachineConfig
  { machineID :: MachineID
  , latency   :: Int
  , timeout   :: Int
  } deriving (Show, Generic, FromJSON, ToJSON)

-- Representation of the DAG in the configuration file
data GraphConfig = GraphConfig
  = EvalNode MachineID String String String [GraphConfig]  -- A single computation node
  | EvalAtomic [GraphConfig]                               -- Atomic block
  | EvalConcurrent [GraphConfig]                           -- Concurrent block
  deriving (Show, Generic, FromJSON, ToJSON)

-- A DAG representing the program control flow
data Graph result
  = Node MachineID (DistributedIO result) [Graph result] -- A computation node with dependencies
  | Atomic [Graph result]                                -- A subgraph that must execute atomically
  | Concurrent [Graph result]                            -- Independent subgraphs that can execute concurrently
  deriving (Show)

-- DistributedIO represents the lifecycle hooks for a distributed computation
data DistributedIO result = DistributedIO
  { lock     :: MachineID -> IO ()                 -- Lock resources before performing the action
  , commit   :: MachineID -> IO result             -- Commit resources (perform the main action)
  , rollback :: MachineID -> IO ()                 -- Rollback changes if needed
  , free     :: MachineID -> Notification -> IO () -- Free resources and notify machines about the outcome
  }

-- Converts a configuration object into a global Graph
convertGraph :: GraphConfig -> [MachineConfig] -> Graph Notification
convertGraph (EvalNode machine resource rollbackMessage notifyMessage dependencies) machineConfigs =
  let config = case lookup machine (map (\mc -> (machineID mc, mc)) machineConfigs) of
                 Just mc -> mc
                 Nothing -> error $ "Configuration not found for machine " ++ machine
  in Node machine (distributedAction resource rollbackMessage notifyMessage config) (map (`convertGraph` machineConfigs) dependencies)
convertGraph (EvalAtomic subgraphs) machineConfigs = Atomic (map (`convertGraph` machineConfigs) subgraphs)
convertGraph (EvalConcurrent subgraphs) machineConfigs = Concurrent (map (`convertGraph` machineConfigs) subgraphs)

-- Split the global Graph into subgraphs for each machine
splitGraphByMachine :: Graph result -> [(MachineID, Graph result)]
splitGraphByMachine graph = case graph of
  Node machine computation dependencies ->
    [(machine, Node machine computation dependencies)] ++ concatMap splitGraphByMachine dependencies
  Atomic subgraphs -> concatMap splitGraphByMachine subgraphs
  Concurrent subgraphs -> concatMap splitGraphByMachine subgraphs
