module DistributedIO.Core
  ( DistributedIO(..),
    distributedAction,
  ) where

-- A type to represent a machine identifier
type MachineID = String

-- A type for distributed IO computations
data DistributedIO a = DistributedIO
  { runDistributed :: MachineID -> IO a -- Execute the computation on a given machine
  }

-- Create a distributed computation
distributedAction :: String -> DistributedIO String
distributedAction input = DistributedIO $ \machineID -> simulateMachine machineID input
