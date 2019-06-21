{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE TypeApplications      #-}
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
  , module Data.Proxy
  ) where

import           Control.Lens
import           Control.Monad                    (void)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State        (execStateT, StateT)
import           Data.Proxy

class Monad f => Modifiable a f where
  modify :: (a -> f a) -> f a
  modify f = get >>= f >>= \a -> put a >> return a

  get :: f a
  get = modify pure

  put :: a -> f ()
  put a = modify_ (pure . const a)

  {-# MINIMAL modify | get, put #-}

  modify_ :: (a -> f a) -> f ()
  modify_ = void . modify

  modifyStatefully :: StateT a f () -> f a
  modifyStatefully = modify . execStateT

  modifyStatefully_ :: StateT a f () -> f ()
  modifyStatefully_ = void . modifyStatefully



class Has b a where
  this :: Lens' b a

instance a `Has` a where
  this = lens id (const id)

instance (Identity a) `Has` a where
  this = lens runIdentity (const Identity)



class Accessible a f where
  access :: f a

accesses :: (Functor f, Accessible a f) => (a -> b) -> f b
accesses = flip fmap access

class Inputs f a where
  input :: f a

inputs :: (Functor f, Inputs f a) => (a -> b) -> f b
inputs f = f <$> input

class Outputs f a where
  output :: a -> f ()

genericOutputsStringIO :: MonadIO m => String -> m ()
genericOutputsStringIO = liftIO . putStrLn
