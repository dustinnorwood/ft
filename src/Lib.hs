{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Lib
    ( Alters(..)
    , ffor
    , module Data.Proxy
    ) where

import Control.Monad             (join, void)
import Control.Monad.Trans.State (evalStateT, execStateT, StateT)
import Data.Maybe
import Data.Proxy
import Data.Traversable          (for)

ffor :: (Applicative f, Monad t, Traversable t) => t a -> (a -> f (t b)) -> f (t b)
ffor t = fmap join . for t

class Monad m => Alters a m where
  type Key a
  alter :: Proxy a -> Key a -> (Maybe a -> m (Maybe a)) -> m (Maybe a)
  {-# MINIMAL alter #-}

  update :: Proxy a -> Key a -> (a -> m (Maybe a)) -> m (Maybe a)
  update p k = alter p k . flip ffor

  updateState :: Proxy a -> Key a -> StateT a m (Maybe a) -> m (Maybe a)
  updateState p k = update p k . evalStateT

  modify :: Proxy a -> Key a -> (a -> m a) -> m a
  modify p k f = fmap fromJust $ update p k (fmap Just . f)

  modifyState :: Proxy a -> Key a -> StateT a m () -> m a
  modifyState p k = modify p k . execStateT

  repsert :: Proxy a -> Key a -> (Maybe a -> m a) -> m a
  repsert p k f = fmap fromJust $ alter p k (fmap Just . f)

  insert :: Proxy a -> Key a -> a -> m ()
  insert p k a = alter_ p k (return . const (Just a))

  get :: Proxy a -> Key a -> m (Maybe a)
  get p k = alter p k return

  alter_ :: Proxy a -> Key a -> (Maybe a -> m (Maybe a)) -> m ()
  alter_ p k = void . alter p k

  update_ :: Proxy a -> Key a -> (a -> m (Maybe a)) -> m ()
  update_ p k = void . update p k

  updateState_ :: Proxy a -> Key a -> StateT a m (Maybe a) -> m ()
  updateState_ p k = void . updateState p k

  modify_ :: Proxy a -> Key a -> (a -> m a) -> m ()
  modify_ p k = void . modify p k

  modifyState_ :: Proxy a -> Key a -> StateT a m () -> m ()
  modifyState_ p k = void . modifyState p k

  repsert_ :: Proxy a -> Key a -> (Maybe a -> m a) -> m ()
  repsert_ p k = void . repsert p k
