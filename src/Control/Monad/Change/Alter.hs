{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Control.Monad.Change.Alter
  ( Alters(..)
  , alter_
  , update
  , update_
  , updateStatefully_
  , updateStatefully
  , adjust
  , adjust_
  , adjustStatefully
  , adjustStatefully_
  , repsert
  , repsert_
  , lookup
  , insert
  , delete
  ) where

import Control.Monad              (void)
import Control.Monad.Change.Proxy (X(..))
import Control.Monad.Trans.State  (evalStateT, execStateT, StateT)
import Data.Maybe
import Prelude                    hiding (lookup)

class Applicative f => Alters k a f where
  alter :: X a -> k -> (Maybe a -> f (Maybe a)) -> f (Maybe a)

alter_ :: (k `Alters` a) f => X a -> k -> (Maybe a -> f (Maybe a)) -> f ()
alter_ p k = void . alter p k

update :: (k `Alters` a) f => X a -> k -> (a -> f (Maybe a)) -> f (Maybe a)
update p k f = alter p k $ \case
  Just a -> f a
  Nothing -> pure Nothing

update_ :: (k `Alters` a) f => X a -> k -> (a -> f (Maybe a)) -> f ()
update_ p k = void . update p k

updateStatefully :: (Monad m, (k `Alters` a) m) => X a -> k -> StateT a m (Maybe a) -> m (Maybe a)
updateStatefully p k = update p k . evalStateT

updateStatefully_ :: (Monad m, (k `Alters` a) m) => X a -> k -> StateT a m (Maybe a) -> m ()
updateStatefully_ p k = void . updateStatefully p k

adjust :: (k `Alters` a) m => X a -> k -> (a -> m a) -> m a
adjust p k f = fmap fromJust $ update p k (fmap Just . f)

adjust_ :: (k `Alters` a) f => X a -> k -> (a -> f a) -> f ()
adjust_ p k = void . adjust p k

adjustStatefully :: (Monad m, (k `Alters` a) m) => X a -> k -> StateT a m () -> m a
adjustStatefully p k = adjust p k . execStateT

adjustStatefully_ :: (Monad m, (k `Alters` a) m) => X a -> k -> StateT a m () -> m ()
adjustStatefully_ p k = void . adjustStatefully p k

repsert :: (k `Alters` a) f => X a -> k -> (Maybe a -> f a) -> f a
repsert p k f = fmap fromJust $ alter p k (fmap Just . f)

repsert_ :: (k `Alters` a) f => X a -> k -> (Maybe a -> f a) -> f ()
repsert_ p k = void . repsert p k

lookup :: (k `Alters` a) f => X a -> k -> f (Maybe a)
lookup p k = alter p k pure

insert :: (k `Alters` a) f => X a -> k -> a -> f ()
insert p k a = alter_ p k (pure . const (Just a))

delete :: (k `Alters` a) f => X a -> k -> f ()
delete p k = alter_ p k (pure . const Nothing)
