{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Monad.Change.Modify
  ( Modifiable(..)
  , modify_
  , modifyStatefully
  , modifyStatefully_
  , get
  , put
  ) where

import Control.Monad              (void)
import Control.Monad.Change.Proxy (X(..))
import Control.Monad.Trans.State  (execStateT, StateT)

class Applicative f => Modifiable a f where
  modify :: X a -> (a -> f a) -> f a

modify_ :: Modifiable a f => X a -> (a -> f a) -> f ()
modify_ p = void . modify p

modifyStatefully :: (Monad m, Modifiable a m) => X a -> StateT a m () -> m a
modifyStatefully p = modify p . execStateT

modifyStatefully_ :: (Monad m, Modifiable a m) => X a -> StateT a m () -> m ()
modifyStatefully_ p = void . modifyStatefully p

get :: Modifiable a f => X a -> f a
get p = modify p pure

put :: Modifiable a f => X a -> a -> f ()
put p a = modify_ p (pure . const a)
