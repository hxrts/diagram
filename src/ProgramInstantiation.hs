{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module ProgramInstantiation (EnvironmentConfig(..), MachineConfig(..), Graph(..), GraphConfig(..), Notification, DistributedIO(..), convertGraph, splitGraphByMachine, MachineID) where

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)
import Control.Concurrent (threadDelay)

type MachineID = String
type Notification = String

-- All machines and the DAG connecting them
data EnvironmentConfig = EnvironmentConfig
  { machines :: [MachineConfig]               -- List of available machines with their latencies and timeouts
  , graph    :: GraphConfig                   -- Definition of the DAG
  } deriving (Show, Generic, FromJSON, ToJSON)

-- Configuration for each machine
data MachineConfig = MachineConfig
  { machineID :: MachineID
  , latency   :: Int
  , timeout   :: Int
  } deriving (Show, Generic, FromJSON, ToJSON)

-- Representation of the DAG in the configuration file
data GraphConfig
  = EvalNode MachineID String String String [GraphConfig]  -- A single computation node
  | EvalAtomic [GraphConfig]                               -- Atomic block
  | EvalConcurrent [GraphConfig]                           -- Concurrent block
  deriving (Show, Generic, FromJSON, ToJSON)

-- A DAG representing the program control flow
data Graph result
  = Node MachineID (DistributedIO result) [Graph result] -- A computation node with dependencies
  | Atomic [Graph result]                                -- A subgraph that must execute atomically
  | Concurrent [Graph result]                            -- Independent subgraphs that can execute concurrently

instance Show (Graph result) where
  show (Node machine _ dependencies) = "Node " ++ show machine ++ " " ++ show dependencies
  show (Atomic subgraphs) = "Atomic " ++ show subgraphs
  show (Concurrent subgraphs) = "Concurrent " ++ show subgraphs

-- DistributedIO represents the lifecycle hooks for a distributed computation
data DistributedIO result = DistributedIO
  { lock     :: MachineID -> IO ()                 -- Lock resources before performing the action
  , commit   :: MachineID -> IO result             -- Commit resources (perform the main action)
  , rescind  :: MachineID -> IO ()                 -- Rescind changes if needed
  , free     :: MachineID -> Notification -> IO () -- Free resources and notify machines about the outcome
  }

instance Show (DistributedIO result) where
  show _ = "DistributedIO"

-- Converts configuration structure into a global Graph
convertGraph :: GraphConfig -> [MachineConfig] -> Graph Notification
convertGraph (EvalNode machine resource rescindMessage notifyMessage dependencies) [machineConfig] =
  let config = case lookup machine (map (\mc -> (machineID mc, mc)) [machineConfig]) of
                 Just mc -> mc
                 Nothing -> error $ "Configuration not found for machine " ++ machine
  in Node machine (distributedAction resource rescindMessage notifyMessage config) (map (`convertGraph` [machineConfig]) dependencies)
convertGraph (EvalAtomic subgraphs) [machineConfig] = Atomic (map (`convertGraph` [machineConfig]) subgraphs)
convertGraph (EvalConcurrent subgraphs) [machineConfig] = Concurrent (map (`convertGraph` [machineConfig]) subgraphs)

-- Split the global Graph into subgraphs for each machine
splitGraphByMachine :: Graph result -> [(MachineID, Graph result)]
splitGraphByMachine graph = case graph of
  Node machine computation dependencies ->
    [(machine, Node machine computation dependencies)] ++ concatMap splitGraphByMachine dependencies
  Atomic subgraphs -> concatMap splitGraphByMachine subgraphs
  Concurrent subgraphs -> concatMap splitGraphByMachine subgraphs

-- Create a DistributedIO action
distributedAction :: String -> String -> String -> MachineConfig -> DistributedIO Notification
distributedAction resource rescindMessage notifyMessage config = DistributedIO
  { lock = \mID -> putStrLn $ "Machine " ++ mID ++ " locking: " ++ resource
  , free = \mID message -> putStrLn $ "Machine " ++ mID ++ " freeing: " ++ resource ++ " with message: " ++ message
  , commit = \mID -> do
      putStrLn $ "Machine " ++ mID ++ " committing: " ++ resource
      threadDelay (latency config)
      return $ "Committed by " ++ mID ++ ": " ++ resource
  , rescind = \mID -> putStrLn $ "Machine " ++ mID ++ " rescinding: " ++ rescindMessage
  }