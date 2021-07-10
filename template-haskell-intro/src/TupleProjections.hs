{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
module TupleProjections where

import Language.Haskell.TH
import Optics.TH

simpleFun :: Name -> [Pat] -> Exp -> Dec
simpleFun name pats rhs = FunD name [Clause pats (NormalB rhs) []]


buildProjections :: Q [Dec]
buildProjections = traverse build_p [1, 2]
  where
    fname n = mkName $ "p2_" ++ show n
    argString k = "a" ++ show k
    argStrings = map argString [1, 2]
    build_p n = do
        argNames <- traverse newName argStrings
        let args = map VarP argNames
        return $ simpleFun (fname n) [TupP args] (VarE (argNames !! (n -1)))


data Animal
  = Cat { _age  :: Int
        , _name :: String
        }
  | Dog { _age    :: Int
        , _absurd :: forall a b. a -> b
        }
makeLenses ''Animal


mkPow :: Int -> TExpQ (Int -> Int)
mkPow 0 = [|| const 1 ||]
mkPow n = [|| \x -> x * $$(mkPow (n-1)) x ||]