{-# LANGUAGE TypeApplications #-}

module EitherDemo where

import Person (Person (..))
import Relude (readMaybe)

import Data.Char (isAlpha)
import Data.UUID.Types (UUID)
import qualified Data.UUID.Types as UUID


parsePerson :: (String, String, String) -> Either String Person
parsePerson (uuidStr, nameStr, ageStr) =
    Person
        <$> parseUUID uuidStr
        <*> parseName nameStr
        <*> parseAge ageStr


parseUUID :: String -> Either String UUID
parseUUID uuidStr = case UUID.fromString uuidStr of
    Nothing -> Left $ "Not a valid UUID: " <> uuidStr
    Just uuid -> Right uuid


parseName :: String -> Either String String
parseName nameStr
    | not (all isAlpha nameStr) = Left $ "Name " <> nameStr <> " contains non-alphabetic characters"
    | null nameStr = Left "Name is empty"
    | otherwise = Right nameStr


parseAge :: String -> Either String Int
parseAge ageStr = case readMaybe ageStr of
    Just age
        | age >= 18 -> Right age
        | otherwise -> Left $ "Age must be >= 18, but was " <> show age
    Nothing -> Left $ "Failed to parse age string: " <> ageStr
