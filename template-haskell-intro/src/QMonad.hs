module QMonad where

import Control.Monad.IO.Class (liftIO)
import Data.Time.Calendar (DayOfWeek (Thursday), dayOfWeek)
import Data.Time.Clock (UTCTime (utctDay), getCurrentTime)
import Language.Haskell.TH (Exp (LitE), Lit (IntegerL), Q)


{- | You  can run arbitrary IO code in the Q monad

This will produce literal 1 on Thursday, but will fail to compile on other days.

demo :: Int
demo = $(oneOnThursday)
-}
oneOnThursday :: Q Exp
oneOnThursday = do
    currentTime <- liftIO getCurrentTime
    let weekDay = dayOfWeek $ utctDay currentTime
    case weekDay of
        Thursday -> pure $ LitE $ IntegerL 1
        _ -> fail $ "Sorry, this only compiles on Thursday"