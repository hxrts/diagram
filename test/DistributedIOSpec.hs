-- test/DistributedIOSpec.hs
module DistributedIOSpec (spec) where

import Test.Hspec
import DistributedIO.Core (DistributedIO(..), distributedAction)

spec :: Spec
spec = do
  describe "distributedAction" $ do
    it "creates a DistributedIO action that produces the correct output" $ do
      let action = distributedAction "Sample Input"
      result <- runDistributed action "TestMachine"
      result `shouldBe` "Output from TestMachine for input: Sample Input"
