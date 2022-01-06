module HooksSpec where

import Control.Concurrent (threadDelay)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, writeTVar)
import GHC.IO (bracket, unsafePerformIO)
import Test.Hspec


spec :: Spec
spec =
    before initConnection $
        --before_ (putStrLn "hi") $
        beforeWith (\(Connection i) -> pure $ even i) $
            --after_ (putStrLn "bye") $
            --around withConnection $
            --aroundAll withConnection $
            describe "Hooks demo" $ do
                describe "Group A" $ do
                    it "a1" $ \a -> print a >> putStrLn "a1"
                    it "a2" $ \a -> putStrLn "a2"
                describe "Group B" $ do
                    it "b1" $ \a -> putStrLn "b1"
                    it "b2" $ \a -> putStrLn "b2"


newtype Connection
    = Connection Int
    deriving (Show)


getCounter :: IO Int
getCounter = atomically $ do
    i <- readTVar counter
    writeTVar counter (i + 1)
    pure i


initConnection :: IO Connection
initConnection = do
    i <- getCounter
    putStrLn $ "Initialized Connection " <> show i
    pure (Connection i)


destroyConnection :: Connection -> IO ()
destroyConnection (Connection i) =
    putStrLn $ "Destroyed connection " <> show i


withConnection :: (Connection -> IO ()) -> IO ()
-- Or using hspec's type synonym:
-- withConnection :: ActionWith Connection -> IO ()
withConnection =
    bracket initConnection destroyConnection


{-# NOINLINE counter #-}
counter :: TVar Int
counter = unsafePerformIO $ newTVarIO 0
