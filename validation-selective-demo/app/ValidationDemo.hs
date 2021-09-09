module ValidationDemo where

import Data.Char (isAlpha)
import Data.UUID.Types (UUID)
import qualified Data.UUID.Types as UUID
import Person (Person (..))
import Relude (NonEmpty, readMaybe)
import Validation


type Val = Validation (NonEmpty String)


parsePerson :: (String, String, String) -> Val Person
parsePerson (uuidStr, nameStr, ageStr) =
    Person
        <$> parseUUID uuidStr
        <*> parseName nameStr
        <*> parseAge ageStr


parseUUID :: String -> Val UUID
parseUUID uuidStr = case UUID.fromString uuidStr of
    Nothing -> failure $ "Not a valid UUID: " <> uuidStr
    Just uuid -> Success uuid


parseName :: String -> Val String
parseName nameStr
    | not (all isAlpha nameStr) = failure $ "Name " <> nameStr <> " contains non-alphabetic characters"
    | null nameStr = failure "Name is empty"
    | otherwise = Success nameStr


parseAge :: String -> Val Int
parseAge ageStr = case readMaybe ageStr of
    Just age
        | age >= 18 -> Success age
        | otherwise -> failure $ "Age must be >= 18, but was " <> show age
    Nothing -> failure $ "Failed to parse age string: " <> ageStr
