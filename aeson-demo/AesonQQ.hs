{-# LANGUAGE QuasiQuotes #-}

module AesonQQ (exampleVal) where

import Data.Aeson (Value)
import Data.Aeson.QQ (aesonQQ)


exampleVal :: Value
exampleVal = [aesonQQ| {name : #{name}, age : #{age}} |]


age :: Int
age = 50


name :: String
name = "Joe"