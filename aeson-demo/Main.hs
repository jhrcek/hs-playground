{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main where

--import qualified AesonQQ
import Data.Aeson

import Data.Aeson.QQ
import GHC.Generics (Generic)


--import Data.Aeson.TH

main :: IO ()
main = do
    let person = Person "Jan" 50
    print person
    print $ encode person

    let colorsJson = encode [Red 15, Blue 15]

    print colorsJson
    print (eitherDecode colorsJson :: Either String [Color])
    print $ eitherDecode @[Color] colorsJson


data Person = Person
    { _name :: String
    , _age :: Int
    }
    deriving (Show, Generic)


data Color
    = Red Int
    | Green
    | Blue Int
    deriving (Generic, Show)


-- instance ToJSON Person where
--     toJSON :: Person -> Value
--     toJSON (Person n a) =
--         object
--             [ "name" .= toJSON n
--             , "age" .= toJSON a
--             ]

instance ToJSON Person


instance ToJSON Color


myOptions :: Options
myOptions = defaultOptions{sumEncoding = defaultTaggedObject}


instance FromJSON Color where
    parseJSON = genericParseJSON myOptions


exampleVal :: Value
exampleVal = [aesonQQ| {"name":"Joe", "age": 50 } |]


-- $(deriveJSON defaultOptions 'Person)
