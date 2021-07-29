{-# LANGUAGE TemplateHaskell #-}

module Expressions where

import Language.Haskell.TH (
    Exp (LamE, LitE, UInfixE, VarE),
    Lit (IntegerL),
    Pat (VarP),
    Q,
    newName,
 )


{- Splicing this expression ...

x = $(one)

... will lead to same result as if we had written this code:

x = 1
-}
one :: Q Exp
one = pure $ LitE (IntegerL 1)


{- | Generates expression:

\x -> x
-}
identity :: Q Exp
identity = do
    x <- newName "x"
    pure $ LamE [VarP x] (VarE x)


{- | Generates expression:

\x -> x + 1
-}
addOne :: Q Exp
addOne = do
    x <- newName "x"
    pure $
        LamE
            [VarP x]
            (UInfixE (VarE x) (VarE '(+)) (LitE (IntegerL 1)))


{- | An alternative to writing AST data type values "by hand" is to use
quotation syntax.

Haskell Parser will parse these and will produce value of appropriate type
- OR fail with syntax error at compile time
-}
addOne2 :: Q Exp
addOne2 = [e|\x -> x + 1|]
