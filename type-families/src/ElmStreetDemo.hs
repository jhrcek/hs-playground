{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}

module ElmStreetDemo where

import Elm.Generic (Elm)
import GHC.Generics (Generic)


data Foo a = Foo deriving (Generic)
data Foo2 a b c d e f g h i j = Foo2 deriving (Generic)
data Bar = Bar Int Int Int Int Int Int Int Int Int deriving (Generic)
data Quux = Quux1 {q1 :: Int} | Quux2 {q2 :: Bool}

-- All of the below instances will fail to compile,
-- because the default implementation of "toElmDefinition" uses bunch of type families
-- to constraint the set of types for which automatic instance can be generated
--
-- instance Elm ((Foo2 a b c d) e f g h i j)
-- instance Elm (Foo Int)
-- instance Elm Bar
-- instance Elm Quux
