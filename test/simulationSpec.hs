-- test/SimulationSpec.hs
module SimulationSpec (spec) where

import Test.Hspec
import DistributedIO.Core (simulateMachine)

spec :: Spec
spec = do
  describe "simulateMachine" $ do
    it "processes the input and produces the correct output" $ do
      let machineID = "MachineX"
      let input = "Test Input"
      result <- simulateMachine machineID input
      result `shouldBe` "Output from MachineX for input: Test Input"
