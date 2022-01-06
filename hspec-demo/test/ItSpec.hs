{-# LANGUAGE ScopedTypeVariables #-}

module ItSpec where

import Control.Exception
import Test.Hspec
import Test.Hspec.QuickCheck (modifyMaxSuccess, prop)
import Test.QuickCheck


spec :: Spec
spec =
    describe "What can be passed to `it`" $ do
        describe "Bool" $ do
            it "True means success" True
            xit "False means failure" False
            it "2+2" $ 2 + 2 == 4

        describe "QuickCheck.Property" $
            describe "Integer addition" $ do
                prop "is commutative" $
                    (\x y -> (x :: Int) + y == y + x)
                prop "has 0 as identity element" $
                    (\x -> 0 + x == (x :: Int))

        describe "Expectation" $ do
            -- `Expectation` is just type synonym for HUnit's `Assertion`, which is type synonym for `IO ()`
            it "passes if there's no exception" $
                (pure () :: IO ())
            xit "fails when exception is thrown" $
                (throwIO (userError "Bad") :: IO ())
            it "Mostly we rely on helper functions from hspec-expectation to throw exception" $
                1 + 1 `shouldBe` 2
            it "getLine" $
                pure () `shouldReturn` ()
