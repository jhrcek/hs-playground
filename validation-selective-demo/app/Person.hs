module Person where

import Data.UUID.Types (UUID)


data Person = Person
    { personUuid :: UUID
    , personName :: String
    , personAge :: Int
    }
    deriving (Show)
