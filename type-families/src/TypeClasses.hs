{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module TypeClasses where
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Data.Data


-- Class is declaring a type of records that are conceptulally similar to "type Boolsey a = { toBool :: a -> Bool }"
class Boolsey a where
    toBool :: a -> Bool

-- Class instances are live values of the record type "boolseyBool = { toBool = \b -> b }"
instance Boolsey Bool where
    toBool b = b


instance Boolsey Int where
    toBool x = x /= 0


class StrLen (s :: Symbol) where
    strLen :: Proxy s -> Int

-- instance StrLen "dog" where
--   strLen _ = 3

instance KnownSymbol s => StrLen s where
    strLen proxy = length (symbolVal proxy)



main :: IO ()
main = do
    -- print @Int 1
    -- print @Bool True

    print (toBool True)
    print (toBool (1 :: Int))


