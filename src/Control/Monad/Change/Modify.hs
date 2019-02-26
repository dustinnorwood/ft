{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Monad.Change.Modify
  ( Modifiable(..)
  ) where

import Control.Monad              (void)
import Control.Monad.Change.Proxy (X(..))
import Control.Monad.Trans.State  (execStateT, StateT)

class Applicative f => Modifiable a f where
  modify :: X a -> (a -> f a) -> f a
  {-# MINIMAL modify #-}

  modify_ :: Modifiable a f => X a -> (a -> f a) -> f ()
  modify_ p = void . modify p

  modifyStatefully :: Monad f => X a -> StateT a f () -> f a
  modifyStatefully p = modify p . execStateT

  modifyStatefully_ :: Monad f => X a -> StateT a f () -> f ()
  modifyStatefully_ p = void . modifyStatefully p

  get :: Modifiable a f => X a -> f a
  get p = modify p pure

  put :: Modifiable a f => X a -> a -> f ()
  put p a = modify_ p (pure . const a)
