{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices -dsuppress-uniques -dsuppress-all #-}

module AesonDemo where

import Data.Aeson (defaultOptions)
import Data.Aeson.TH (defaultOptions, deriveJSON)


data Person = Person
    { name :: String
    , age :: Int
    }
    deriving (Show)


-- Using TH to derive ToJSON / FromJSON
-- Note that we don't have to use the $(..) splice syntax
-- when splicing declarations at top level
deriveJSON defaultOptions ''Person
