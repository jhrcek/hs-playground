{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module QuasiQuoters (
    NumericString,
    demo,
    numStr,
    mkNumericString,
) where

import Data.Char (isDigit)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax


{- Show how QQ works by just returning user's String -}
demo :: QuasiQuoter
demo =
    QuasiQuoter
        { quoteExp = \str -> stringE $ "You entered string: " ++ str
        , -- '' below is TH syntax which gives us Name of the Int type.
          -- You can think any type which is in scope.
          -- In this case Int is coming from implicit Prelude import
          quoteType = \_ -> pure $ ConT ''Int
        , quotePat = error "demo QuasiQuoter cannot be used in pattern context"
        , quoteDec = error "demo QuasiQuoter cannot be used in declaration context"
        }


newtype NumericString
    = NumericString String
    deriving (Show, Lift)


mkNumericString :: String -> Maybe NumericString
mkNumericString str
    | all isDigit str = Just (NumericString str)
    | otherwise = Nothing


numStr :: QuasiQuoter
numStr =
    QuasiQuoter
        { quoteExp = \str -> case mkNumericString str of
            Just ns -> lift ns
            Nothing -> fail $ "Failed to create NumericString, input has non-numeric characters: " ++ str
        , quoteType = error "numStr QuasiQuoter cannot be used in type context"
        , quotePat = error "numStr QuasiQuoter cannot be used in pattern context"
        , quoteDec = error "numStr QuasiQuoter cannot be used in declaration context"
        }

-- Look at easy to understand examples of quasi quoters
-- https://hackage.haskell.org/package/postgresql-simple-0.6.4/docs/Database-PostgreSQL-Simple-SqlQQ.html#v:sql
-- https://hackage.haskell.org/package/sqlite-simple-0.4.18.0/docs/Database-SQLite-Simple-QQ.html#v:sql
-- https://hackage.haskell.org/package/aeson-1.5.6.0/docs/Data-Aeson-QQ-Simple.html#v:aesonQQ
-- https://github.com/haskell/aeson/pull/857
