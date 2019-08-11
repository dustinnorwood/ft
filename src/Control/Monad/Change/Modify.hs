{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module Control.Monad.Change.Modify
  ( Modifiable(..)
  , Has(..)
  , Accessible(..)
  , accesses
  , Inputs(..)
  , inputs
  , Outputs(..)
  , genericOutputsStringIO
  , Awaits(..)
  , Yields(..)
  , module Data.Proxy
  ) where

import           Control.Lens
import           Control.Monad                    (void, mapM_)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State        (execStateT, StateT)
import           Data.Proxy

class Monad f => Modifiable a f where
  modify :: Proxy a -> (a -> f a) -> f a
  modify p f = get p >>= f >>= \a -> put p a >> return a

  get :: Proxy a -> f a
  get p = modify p pure

  put :: Proxy a -> a -> f ()
  put p a = modify_ p (pure . const a)

  {-# MINIMAL modify | get, put #-}

  modify_ :: Proxy a -> (a -> f a) -> f ()
  modify_ p = void . modify p

  modifyStatefully :: Proxy a -> StateT a f () -> f a
  modifyStatefully p = modify p . execStateT

  modifyStatefully_ :: Proxy a -> StateT a f () -> f ()
  modifyStatefully_ p = void . modifyStatefully p



class Has b a where
  this :: Proxy a -> Lens' b a

instance a `Has` a where
  this _ = lens id (const id)

instance (Identity a) `Has` a where
  this _ = lens runIdentity (const Identity)



class Accessible a f where
  access :: Proxy a -> f a

accesses :: (Functor f, Accessible a f) => Proxy a -> (a -> b) -> f b
accesses = flip fmap . access

class Inputs f a where
  input :: f a

inputs :: (Functor f, Inputs f a) => (a -> b) -> f b
inputs f = f <$> input

class Outputs f a where
  output :: a -> f ()

genericOutputsStringIO :: MonadIO m => String -> m ()
genericOutputsStringIO = liftIO . putStrLn

class Awaits f a where
  await :: f (Maybe a)
  {-# MINIMAL await #-}

  awaitForever :: Monad f => (a -> f ()) -> f ()
  awaitForever f = await >>= \case
    Nothing -> return ()
    Just a -> f a >> awaitForever f

class Yields f a where
  yield :: a -> f ()
  {-# MINIMAL yield #-}

  yieldMany :: Monad f => [a] -> f ()
  yieldMany = mapM_ yield
