module Main where

import Data.Either (partitionEithers)
import Data.Foldable (for_, traverse_)
import Validation (Validation (Failure, Success))

import qualified EitherDemo as E
import qualified ValidationDemo as V


main :: IO ()
main = do
    putStrLn "\nAttempt 1: using Either"
    let resultEither = traverse E.parsePerson inputData
    -- Issue: we can only see the first error.
    -- After fixing it you have to rerun the program to see the next error.
    -- Very tedious step-by-step process to see/fix all the errors
    print resultEither

    putStrLn "\nAttempt 2: trying partitionEithers to get more errors at once"
    let (errors, persons) = partitionEithers $ fmap E.parsePerson inputData
    -- Issue: nothing is forcing you to look at the errors.
    -- You have to remember to check if the list of errors is not empty,
    -- because you always get list of persons.
    -- Also you still only get the first error for each person
    case errors of
        [] ->
            -- do something vith the list of persons
            pure ()
        _ : _ ->
            for_ errors putStrLn

    putStrLn "\nAttempt 3 - using Validation to see all the errors at once"
    let resultValidation = traverse V.parsePerson inputData
    case resultValidation of
        -- The list of errors is guaranteed non-empty, so we don't have to deal with "empty error list" case
        Failure nonEmptyListOfErrors ->
            for_ nonEmptyListOfErrors putStrLn
        Success pers ->
            -- do something with list of valid persons
            pure ()


inputData :: [(String, String, String)]
inputData =
    [ ("550e8400-e29b-41d4-a716-446655440000", "Ja n", "50")
    , ("123e4567-e89b-12d3-a456-426614174000", "", "50")
    , ("23e4567-e89b-12d3-a456-426614174000", "Jim", "10")
    , ("\"123e4567-e89b-12d3-a456-426614174000\"", "Jane", "1O")
    ]
