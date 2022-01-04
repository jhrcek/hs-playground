{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices -dsuppress-uniques -dsuppress-all #-}

module Main where

import Declarations
import Expressions
import QMonad
import QuasiQuoters


main :: IO ()
main = pure ()

-- one = $(oneOnThursday)

-- x = [demo|Hello|]

-- y :: [demo|Bye|]
-- y = 1

-- ns :: NumericString
-- ns = [numStr|123|]

-- badNs :: NumericString
-- badNs = [numStr|abc|]
