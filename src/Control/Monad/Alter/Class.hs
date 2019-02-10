{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeOperators          #-}

module Control.Monad.Alter.Class
  ( Alters(..)
  , X(..)
  , update
  , updateStatefully
  , modify
  , modifyStatefully
  , repsert
  , insert
  , delete
  , get
  , alter_
  , update_
  , updateStatefully_
  , modify_
  , modifyStatefully_
  , repsert_
  ) where

import Control.Monad             (void)
import Control.Monad.Trans.State (evalStateT, execStateT, StateT)
import Data.Maybe

data X a = P

class Applicative f => Alters k a f where
  alter :: X a -> k -> (Maybe a -> f (Maybe a)) -> f (Maybe a)
  {-# MINIMAL alter #-}

update :: (k `Alters` a) m => X a -> k -> (a -> m (Maybe a)) -> m (Maybe a)
update p k f = alter p k $ \case
  Just a -> f a
  Nothing -> pure Nothing

updateStatefully :: (Monad m, (k `Alters` a) m) => X a -> k -> StateT a m (Maybe a) -> m (Maybe a)
updateStatefully p k = update p k . evalStateT

modify :: (k `Alters` a) m => X a -> k -> (a -> m a) -> m a
modify p k f = fmap fromJust $ update p k (fmap Just . f)

modifyStatefully :: (Monad m, (k `Alters` a) m) => X a -> k -> StateT a m () -> m a
modifyStatefully p k = modify p k . execStateT

repsert :: (k `Alters` a) f => X a -> k -> (Maybe a -> f a) -> f a
repsert p k f = fmap fromJust $ alter p k (fmap Just . f)

insert :: (k `Alters` a) f => X a -> k -> a -> f ()
insert p k a = alter_ p k (pure . const (Just a))

delete :: (k `Alters` a) f => X a -> k -> f ()
delete p k = alter_ p k (pure . const Nothing)

get :: (k `Alters` a) f => X a -> k -> f (Maybe a)
get p k = alter p k pure

alter_ :: (k `Alters` a) f => X a -> k -> (Maybe a -> f (Maybe a)) -> f ()
alter_ p k = void . alter p k

update_ :: (k `Alters` a) m => X a -> k -> (a -> m (Maybe a)) -> m ()
update_ p k = void . update p k

updateStatefully_ :: (Monad m, (k `Alters` a) m) => X a -> k -> StateT a m (Maybe a) -> m ()
updateStatefully_ p k = void . updateStatefully p k

modify_ :: (k `Alters` a) m => X a -> k -> (a -> m a) -> m ()
modify_ p k = void . modify p k

modifyStatefully_ :: (Monad m, (k `Alters` a) m) => X a -> k -> StateT a m () -> m ()
modifyStatefully_ p k = void . modifyStatefully p k

repsert_ :: (k `Alters` a) f => X a -> k -> (Maybe a -> f a) -> f ()
repsert_ p k = void . repsert p k
