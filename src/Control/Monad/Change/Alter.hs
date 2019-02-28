{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Control.Monad.Change.Alter
  ( Alters(..)
  ) where

import           Control.Monad              (void)
import           Control.Monad.Trans.State  (evalStateT, execStateT, StateT)
import           Data.Map.Strict            (Map)
import qualified Data.Map.Strict            as M
import           Data.Maybe
import           Data.Proxy
import           Prelude                    hiding (lookup)

class (Ord k, Applicative f) => Alters k a f where
  alterMany :: Proxy a -> [k] -> (Map k a -> f (Map k a)) -> f (Map k a)
  {-# MINIMAL alterMany #-}

  alterMany_ :: Proxy a -> [k] -> (Map k a -> f (Map k a)) -> f ()
  alterMany_ p ks = void . alterMany p ks

  alter :: Proxy a -> k -> (Maybe a -> f (Maybe a)) -> f (Maybe a)
  alter x k f = M.lookup k <$> alterMany x [k] (M.alterF f k)

  alter_ :: Proxy a -> k -> (Maybe a -> f (Maybe a)) -> f ()
  alter_ p k = void . alter p k

  update :: Proxy a -> k -> (a -> f (Maybe a)) -> f (Maybe a)
  update p k f = alter p k $ \case
    Just a -> f a
    Nothing -> pure Nothing

  update_ :: Proxy a -> k -> (a -> f (Maybe a)) -> f ()
  update_ p k = void . update p k

  updateStatefully :: Monad f => Proxy a -> k -> StateT a f (Maybe a) -> f (Maybe a)
  updateStatefully p k = update p k . evalStateT

  updateStatefully_ :: Monad f => Proxy a -> k -> StateT a f (Maybe a) -> f ()
  updateStatefully_ p k = void . updateStatefully p k

  adjust :: Proxy a -> k -> (a -> f a) -> f a
  adjust p k f = fmap fromJust $ update p k (fmap Just . f)

  adjust_ :: Proxy a -> k -> (a -> f a) -> f ()
  adjust_ p k = void . adjust p k

  adjustStatefully :: Monad f => Proxy a -> k -> StateT a f () -> f a
  adjustStatefully p k = adjust p k . execStateT

  adjustStatefully_ :: Monad f => Proxy a -> k -> StateT a f () -> f ()
  adjustStatefully_ p k = void . adjustStatefully p k

  repsert :: Proxy a -> k -> (Maybe a -> f a) -> f a
  repsert p k f = fmap fromJust $ alter p k (fmap Just . f)

  repsert_ :: Proxy a -> k -> (Maybe a -> f a) -> f ()
  repsert_ p k = void . repsert p k

  lookupMany :: Proxy a -> [k] -> f (Map k a)
  lookupMany p k = alterMany p k pure

  lookup :: Proxy a -> k -> f (Maybe a)
  lookup p k = alter p k pure

  insert :: Proxy a -> k -> a -> f ()
  insert p k a = alter_ p k (pure . const (Just a))

  delete :: Proxy a -> k -> f ()
  delete p k = alter_ p k (pure . const Nothing)
