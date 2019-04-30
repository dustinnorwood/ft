{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE UndecidableInstances  #-}

module Control.Monad.Change.Modify
  ( Modifiable(..)
  , module Data.Proxy
  ) where

import           Control.Lens
import           Control.Monad             (void)
import           Control.Monad.IO.Class
import qualified Control.Monad.State.Class as State
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State (execStateT, StateT)
import           Data.IORef
import           Data.Proxy

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



instance (Monad m, State.MonadState s m) => Modifiable s m where
  get _   = State.get
  put _ s = State.put s

instance {-# OVERLAPPING #-} MonadIO m => Modifiable s (ReaderT (IORef s) m) where
  get _   = liftIO . readIORef =<< ask
  put _ s = liftIO . flip writeIORef s =<< ask



class Has a b where
  this :: Proxy a -> Lens' b a

instance {-# OVERLAPPING #-} (Monad m, Has a b) => Modifiable a (StateT b m) where
  get p   = use (this p)
  put p s = assign (this p) s
