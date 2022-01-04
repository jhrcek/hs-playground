{-# LANGUAGE TemplateHaskell #-}

module Declarations where

import Language.Haskell.TH (
    Body (NormalB),
    Clause (Clause),
    Dec (FunD, SigD),
    Exp (AppE, InfixE, LitE, VarE),
    Lit (IntegerL),
    Pat (LitP, VarP),
    Q,
    Type (AppT, ArrowT, ConT),
    mkName,
 )


factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n -1)


factorialDeclaration1 :: Q [Dec]
factorialDeclaration1 = do
    let factorial = mkName "factorial"
        n = mkName "n"
    pure
        [ SigD factorial (AppT (AppT ArrowT (ConT ''Int)) (ConT ''Int))
        , FunD
            factorial
            [ Clause [LitP (IntegerL 0)] (NormalB (LitE (IntegerL 1))) []
            , Clause [VarP n] (NormalB (InfixE (Just (VarE n)) (VarE '(*)) (Just (AppE (VarE factorial) (InfixE (Just (VarE n)) (VarE '(-)) (Just (LitE (IntegerL 1)))))))) []
            ]
        ]


factorialDeclaration2 :: Q [Dec]
factorialDeclaration2 =
    [d|
        factorial :: Int -> Int
        factorial 0 = 1
        factorial n = n * factorial (n -1)
        |]
