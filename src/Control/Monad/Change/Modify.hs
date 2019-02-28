{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Monad.Change.Modify
  ( Modifiable(..)
  ) where

import Control.Monad              (void)
import Control.Monad.Trans.State  (execStateT, StateT)
import Data.Proxy

class Applicative f => Modifiable a f where
  modify :: Proxy a -> (a -> f a) -> f a
  {-# MINIMAL modify #-}

  modify_ :: Modifiable a f => Proxy a -> (a -> f a) -> f ()
  modify_ p = void . modify p

  modifyStatefully :: Monad f => Proxy a -> StateT a f () -> f a
  modifyStatefully p = modify p . execStateT

  modifyStatefully_ :: Monad f => Proxy a -> StateT a f () -> f ()
  modifyStatefully_ p = void . modifyStatefully p

  get :: Modifiable a f => Proxy a -> f a
  get p = modify p pure

  put :: Modifiable a f => Proxy a -> a -> f ()
  put p a = modify_ p (pure . const a)
