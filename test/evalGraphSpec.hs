module EvalGraphSpec (spec) where

import Test.Hspec
import DistributedIO.Core (DistributedIO(..))
import DistributedIO.Code (EvalGraph(..), evaluateGraph)

-- A mock DistributedIO action
mockDistributedAction :: String -> DistributedIO String
mockDistributedAction output = DistributedIO (\_ -> pure output)

spec :: Spec
spec = do
  describe "evaluateGraph" $ do
    it "evaluates a single node graph" $ do
      let graph = Node "MachineA" (mockDistributedAction "Result A") []
      result <- evaluateGraph graph
      result `shouldBe` ["Result A"]

    it "evaluates a graph with dependencies" $ do
      let graph = Node "MachineA" (mockDistributedAction "Root")
                   [ Node "MachineB" (mockDistributedAction "Child 1") []
                   , Node "MachineC" (mockDistributedAction "Child 2") []
                   ]
      result <- evaluateGraph graph
      result `shouldBe` ["Child 1", "Child 2", "Root"]

    it "evaluates a concurrent graph" $ do
      let graph = Concurrent
            [ Node "MachineA" (mockDistributedAction "Action 1") []
            , Node "MachineB" (mockDistributedAction "Action 2") []
            ]
      result <- evaluateGraph graph
      result `shouldMatchList` ["Action 1", "Action 2"] -- Order may vary
