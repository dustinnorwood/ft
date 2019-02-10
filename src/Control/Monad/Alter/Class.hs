{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Control.Monad.Alter.Class
    ( Alters(..)
    , X(..)
    , ffor
    ) where

import Control.Monad             (join, void)
import Control.Monad.Trans.State (evalStateT, execStateT, StateT)
import Data.Maybe
import Data.Traversable          (for)

data X a = P

ffor :: (Applicative f, Monad t, Traversable t) => t a -> (a -> f (t b)) -> f (t b)
ffor t = fmap join . for t

class Monad m => Alters k a m where
  alter :: X a -> k -> (Maybe a -> m (Maybe a)) -> m (Maybe a)
  {-# MINIMAL alter #-}

  update :: X a -> k -> (a -> m (Maybe a)) -> m (Maybe a)
  update p k = alter p k . flip ffor

  updateState :: X a -> k -> StateT a m (Maybe a) -> m (Maybe a)
  updateState p k = update p k . evalStateT

  modify :: X a -> k -> (a -> m a) -> m a
  modify p k f = fmap fromJust $ update p k (fmap Just . f)

  modifyState :: X a -> k -> StateT a m () -> m a
  modifyState p k = modify p k . execStateT

  repsert :: X a -> k -> (Maybe a -> m a) -> m a
  repsert p k f = fmap fromJust $ alter p k (fmap Just . f)

  insert :: X a -> k -> a -> m ()
  insert p k a = alter_ p k (return . const (Just a))

  get :: X a -> k -> m (Maybe a)
  get p k = alter p k return

  alter_ :: X a -> k -> (Maybe a -> m (Maybe a)) -> m ()
  alter_ p k = void . alter p k

  update_ :: X a -> k -> (a -> m (Maybe a)) -> m ()
  update_ p k = void . update p k

  updateState_ :: X a -> k -> StateT a m (Maybe a) -> m ()
  updateState_ p k = void . updateState p k

  modify_ :: X a -> k -> (a -> m a) -> m ()
  modify_ p k = void . modify p k

  modifyState_ :: X a -> k -> StateT a m () -> m ()
  modifyState_ p k = void . modifyState p k

  repsert_ :: X a -> k -> (Maybe a -> m a) -> m ()
  repsert_ p k = void . repsert p k
