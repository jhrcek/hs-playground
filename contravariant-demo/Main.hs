module Main where

import Data.Functor.Contravariant


main :: IO ()
main = pure ()


---------------- Predicate examples ------------------
notEmpty :: Predicate [a]
notEmpty = Predicate (not . null)
{- ^
>>> getPredicate notEmpty []
False

>>> getPredicate notEmpty [1,2,3]
True
-}


positiveInt :: Predicate Int
positiveInt = Predicate (> 0)
{- ^
>>> getPredicate positiveInt 1
True

>>> getPredicate positiveInt (-1)
False
-}


intGreaterOrEqual :: Int -> Predicate Int
intGreaterOrEqual limit = Predicate (>= limit)
{- ^
>>> getPredicate (intGreaterOrEqual 18) 10
False

>>> getPredicate (intGreaterOrEqual 18) 20
True
-}


agePositive :: Predicate Person
agePositive = contramap age positiveInt
{- ^
>>> getPredicate agePositive (Person "Jan" 100)
True
-}


ageAtLeast :: Int -> Predicate Person
ageAtLeast limit = contramap age $ intGreaterOrEqual limit
{- ^
>>> getPredicate (ageAtLeast 18) (Person "Jan" 20)
True
-}


nameLengthAtLeast :: Int -> Predicate Person
nameLengthAtLeast limit =
    contramap (length . name) $ intGreaterOrEqual limit


personValid :: Predicate Person
personValid = ageAtLeast 18 <> nameLengthAtLeast 3
{- ^
>>> getPredicate personValid (Person "Hu" 15)
False

>>> getPredicate personValid (Person "Jan" 20)
True
-}


personValid2 :: Person -> Bool
personValid2 p = age p >= 18 && length (name p) >= 3


--------------- Comparison examples -----------------------

data Person = Person
    { name :: String
    , age :: Int
    }
    deriving (Show)


intComp :: Comparison Int
intComp = Comparison compare
{- ^
>>> sortBy (getComparison intComp) [5,1,3]
[1,3,5]
-}


stringComp :: Comparison String
stringComp = defaultComparison
{- ^
>>> sortBy (getComparison stringComp) ["z", "a"]
["a","z"]
-}


personByAgeComp :: Comparison Person -- Person -> Person -> Ordering
personByAgeComp = contramap age intComp
{- ^
>>> sortBy (getComparison personByAgeComp) [Person "Ada" 10, Person "Zee" 5]
[Person {name = "Zee", age = 5},Person {name = "Ada", age = 10}]
-}


listByLengthComp :: Comparison [a]
listByLengthComp = contramap length intComp
{- ^
>>> (getComparison listByLengthComp) [1,2,3] [1,2]
GT
-}


-- comparePersonsByAge :: Person -> Person -> Ordering
-- comparePersonsByAge pa pb = age pa `compare` age pb

personByNameComp :: Comparison Person
personByNameComp = contramap name stringComp
{- ^
>>> sortBy (getComparison personByNameComp) [Person "Zee" 5, Person "Ada" 10]
[Person {name = "Ada", age = 10},Person {name = "Zee", age = 5}]
-}


personByNameThenAge :: Comparison Person
personByNameThenAge = personByNameComp <> personByAgeComp
{- ^
>>> sortBy (getComparison personByNameThenAge) [Person "A" 100, Person "A" 5, Person  "Zee" 20]
[Person {name = "A", age = 5},Person {name = "A", age = 100},Person {name = "Zee", age = 20}]
-}


personByAgeThenName :: Comparison Person
personByAgeThenName = personByAgeComp <> personByNameComp
{- ^
>>> sortBy (getComparison personByAgeThenName) [Person "A" 100, Person "A" 5, Person  "Zee" 20]
[Person {name = "A", age = 5},Person {name = "Zee", age = 20},Person {name = "A", age = 100}]
-}
