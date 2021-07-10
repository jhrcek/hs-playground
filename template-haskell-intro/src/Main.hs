{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices -dsuppress-uniques  #-}
module Main where

import TupleProjections 


$buildProjections



{-
>>> p2_1 (1, 2)
1
>>> p2_2 (1, 2)
2
-}
main :: IO ()
main = do
    print $ p2_1 (1, 2)
    print $ p2_2 (1, 2)


foo :: Int -> Int
foo = $$(mkPow 3)