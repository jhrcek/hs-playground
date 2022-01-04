module HooksSpec where

import Test.Hspec


spec :: Spec
spec = do
    describe "Group A" $ do
        it "a1" True
        it "a2" True
    describe "Group B" $ do
        it "b1" True
        it "b2" True
