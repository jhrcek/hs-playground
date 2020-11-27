{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative (Applicative (liftA2))
import Control.Monad (replicateM_)
import Control.Monad.IO.Class (MonadIO (..))
import Text.Read (readMaybe)

main :: IO ()
main = do
  putStrLn "Functors compose"
  let wrappedOne :: A (B Int)
      wrappedOne = A (B 1)
  -- Without taking advantage of functor composition
  -- we have to implement fmap "manually"
  print $ fmap (fmap (+1)) wrappedOne
  -- Taking advantage of functor composition
  -- -> getting implementation for fmap "for free"
  print $ fmap (+1) $ Compose wrappedOne

  putStrLn "\nApplicatives compose"
  print $ Compose (A (B show)) <*> Compose wrappedOne

  putStrLn "\nReaderT demo"
  runReader multiHelloPrinter 3

  runReader compositeAction 5

newtype A a = A a deriving Show
newtype B a = B a deriving (Functor, Show)

type MyNewFunctor = Compose (Compose A B) C

instance Functor A where
  fmap f (A x) = A (f x)

instance Applicative A where
  pure x = A x
  A f <*> A x = A (f x)

instance Applicative B where
  pure x = B x
  B f <*> B x = B (f x)

instance Monad A where
  A x >>= f = f x

instance Monad B where
  B x >>= f = f x

newtype C a = C (A (B a))

-- 1. Functors compose, which we can prove
-- by implementing Functor instance for composition of ANY pair of functors
newtype Compose f g a = Compose (f (g a)) deriving Show

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap h (Compose fga) = Compose $ fmap (fmap h) fga

-- 2. Applicatives compose, which we can prove
-- by implementing Applicative instance for composition of ANY pair of applicatives
instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure x = Compose $ pure $ pure x
  (Compose fgfun) <*> (Compose fga) = Compose $ liftA2 ($) <$> fgfun <*> fga



-- 3. Monads do not compose in general
-- try as you might, but you'll never be able to implement
-- Monad instance for composition of any 2 monads IN GENERAL
-- The problems arise when trying to implement (>>=) operator
--
-- For example, given what we have on the left side
-- We can create a value of type `f (g (f (g a)))`
-- by doing something like `fmap (fmap h) fga`
-- but then we're stuck - no way to "swap" the layers
-- to get something like `f (f (g (g a)))` which could be dealt with
-- using `join :: Monad m => m (m a) -> m a`

-- instance (Monad f, Monad g) => Monad (Compose f g) where
--   (>>=) :: Compose f g a -> (a -> Compose f g b) -> Compose f g b
--   (Compose fga) >>= h = fga >>= (\ga -> ga >>= (\a -> ???))


-- 4. Monad transformers are the next best thing we can do by implementing
-- monad composition for SPECIFIC pairs of monads.
-- That is knowing at least one of the monads allows us to know
-- how to implement the "Layer swapping"

newtype MyReaderT r m a = MyReaderT { runReader :: r -> m a }

instance Functor m => Functor (MyReaderT r m) where
  fmap f (MyReaderT funRToMA) = MyReaderT $ \r -> fmap f $ funRToMA r

instance Applicative m => Applicative (MyReaderT r m) where
  pure :: x -> MyReaderT r m x
  pure x = MyReaderT $ \_ -> pure x -- :: m a

  (<*>) :: forall a b. MyReaderT r m (a -> b) -> MyReaderT r m a -> MyReaderT r m b
  MyReaderT rToMAToB <*> MyReaderT rToMA = MyReaderT $ \r ->
    let maToB :: m (a -> b)
        maToB = rToMAToB r

        ma :: m a
        ma = rToMA r

    in maToB <*> ma

instance Monad m => Monad (MyReaderT r m) where
  (>>=) :: MyReaderT r m a -> (a -> MyReaderT r m b) -> MyReaderT r m b
  MyReaderT f >>= g = MyReaderT $ \r -> let ma = f r in ma >>= (\a -> runReader (g a) r)

instance MonadIO m => MonadIO (MyReaderT r m) where
  liftIO ioAct = MyReaderT $ \_ -> liftIO ioAct


multiHelloPrinter :: MyReaderT Int IO ()
multiHelloPrinter = MyReaderT $ \i ->
    replicateM_ i $ putStrLn "Hello"


-- This is a usual helper function that allows us to get
-- easy access to our "environment" withing any ReaderT action
ask :: Monad m => MyReaderT r m r
ask = MyReaderT $ \r -> pure r


readUserNumber :: MyReaderT a IO Int
readUserNumber = do
    str <- liftIO $ do
        putStr "Please enter a number: "
        getLine
    case readMaybe str of
        Nothing -> do
            liftIO $ putStrLn "That wasn't a number! Try again."
            readUserNumber
        Just n -> pure n


compositeAction :: MyReaderT Int IO ()
compositeAction = do
    userInt <- readUserNumber
    -- read our "environment" value
    envInt <- ask
    liftIO $ putStrLn $ "Your number + environment number is " <> show (userInt + envInt)
