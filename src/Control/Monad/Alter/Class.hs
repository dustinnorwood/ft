{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeOperators          #-}

module Control.Monad.Alter.Class
  ( Alters(..)
  , X(..)
  , ffor
  , update
  , updateStatefully
  , modify
  , modifyStatefully
  , repsert
  , insert
  , get
  , alter_
  , update_
  , updateState_
  , modify_
  , modifyState_
  , repsert_
  ) where

import Control.Monad             (join, void)
import Control.Monad.Trans.State (evalStateT, execStateT, StateT)
import Data.Maybe
import Data.Traversable          (for)

data X a = P

ffor :: (Applicative f, Monad t, Traversable t) => t a -> (a -> f (t b)) -> f (t b)
ffor t = fmap join . for t

class Alters k a m where
  alter :: X a -> k -> (Maybe a -> m (Maybe a)) -> m (Maybe a)
  {-# MINIMAL alter #-}

update :: (Monad m, (k `Alters` a) m) => X a -> k -> (a -> m (Maybe a)) -> m (Maybe a)
update p k = alter p k . flip ffor

updateStatefully :: (Monad m, (k `Alters` a) m) => X a -> k -> StateT a m (Maybe a) -> m (Maybe a)
updateStatefully p k = update p k . evalStateT

modify :: (Monad m, (k `Alters` a) m) => X a -> k -> (a -> m a) -> m a
modify p k f = fmap fromJust $ update p k (fmap Just . f)

modifyStatefully :: (Monad m, (k `Alters` a) m) => X a -> k -> StateT a m () -> m a
modifyStatefully p k = modify p k . execStateT

repsert :: (Functor m, (k `Alters` a) m) => X a -> k -> (Maybe a -> m a) -> m a
repsert p k f = fmap fromJust $ alter p k (fmap Just . f)

insert :: (Applicative m, (k `Alters` a) m) => X a -> k -> a -> m ()
insert p k a = alter_ p k (pure . const (Just a))

get :: (Applicative m, (k `Alters` a) m) => X a -> k -> m (Maybe a)
get p k = alter p k pure

alter_ :: (Functor m, (k `Alters` a) m) => X a -> k -> (Maybe a -> m (Maybe a)) -> m ()
alter_ p k = void . alter p k

update_ :: (Monad m, (k `Alters` a) m) => X a -> k -> (a -> m (Maybe a)) -> m ()
update_ p k = void . update p k

updateState_ :: (Monad m, (k `Alters` a) m) => X a -> k -> StateT a m (Maybe a) -> m ()
updateState_ p k = void . updateStatefully p k

modify_ :: (Monad m, (k `Alters` a) m) => X a -> k -> (a -> m a) -> m ()
modify_ p k = void . modify p k

modifyState_ :: (Monad m, (k `Alters` a) m) => X a -> k -> StateT a m () -> m ()
modifyState_ p k = void . modifyStatefully p k

repsert_ :: (Functor m, (k `Alters` a) m) => X a -> k -> (Maybe a -> m a) -> m ()
repsert_ p k = void . repsert p k
