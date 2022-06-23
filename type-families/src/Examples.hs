{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module Examples where

import Data.Kind


-- data Z -- Zero
-- data S n -- Successor

---
-- Zero
-- One = S Z
-- Two = S (S Z)
-- Three = S Two = S (S (S Z))
-- Vec :: * -> * -> *
-- data Vec :: Type -> Type -> Type where
--     Nil :: Vec a Z
--     Cons :: a -> Vec a n -> Vec a (S n)

-- list :: [Int]
-- list = [1, 2, 3]

-- empty :: Vec String Z
-- empty = Nil

-- oneElem :: Vec Int (S Z)
-- oneElem = Cons 1 Nil

-- twoElem :: Vec Int (S (S Z))
-- twoElem = Cons 2 oneElem

-- nonsense :: Vec Int (Maybe Bool)
-- nonsense = undefined

-- >>> :kind Vec
-- Vec :: * -> * -> *
-- What we want: Vec :: * -> Nat -> *

data Nat = Z | S Nat deriving (Show)


intVal :: Nat -> Int
intVal Z = 0
intVal (S n) = 1 + intVal n


-- Thanks to DataKinds we also have
-- Type Z, Type constructor S
-- and Kind Nat

-- data Vec :: Type -> Type -> Type where
--     Nil :: Vec a Z
--     Cons :: a -> Vec a n -> Vec a (S n)

data Vec :: Type -> Nat -> Type where
    Nil :: Vec a Z
    Cons :: a -> Vec a n -> Vec a (S n)


empty :: Vec String Z
empty = Nil


oneElem :: Vec Int (S Z)
oneElem = Cons 1 Nil


twoElem :: Vec Int (S (S Z))
twoElem = Cons 2 oneElem


nonsense :: Vec Int Z
nonsense = undefined

