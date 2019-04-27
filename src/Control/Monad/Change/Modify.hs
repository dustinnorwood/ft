{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Monad.Change.Modify
  ( Modifiable(..)
  , module Data.Proxy
  ) where

import Control.Monad              (void)
import Control.Monad.Trans.State  (execStateT, StateT)
import Data.Proxy

class Monad f => Modifiable a f where
  modify :: Proxy a -> (a -> f a) -> f a
  modify p f = get p >>= f >>= \a -> put p a >> return a

  get :: Modifiable a f => Proxy a -> f a
  get p = modify p pure

  put :: Modifiable a f => Proxy a -> a -> f ()
  put p a = modify_ p (pure . const a)

  {-# MINIMAL modify | get, put #-}

  modify_ :: Modifiable a f => Proxy a -> (a -> f a) -> f ()
  modify_ p = void . modify p

  modifyStatefully :: Proxy a -> StateT a f () -> f a
  modifyStatefully p = modify p . execStateT

  modifyStatefully_ :: Proxy a -> StateT a f () -> f ()
  modifyStatefully_ p = void . modifyStatefully p
