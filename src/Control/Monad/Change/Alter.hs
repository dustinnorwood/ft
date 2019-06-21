{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Control.Monad.Change.Alter
  ( Alters(..)
  , Maps(..)
  , Selectable(..)
  , Replaceable(..)
  , Removable(..)
  ) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Trans.State   (evalStateT, execStateT, StateT)
import           Data.Default
import qualified Data.IntMap                 as IM
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as M
import           Data.Maybe
import           Prelude                     hiding (lookup)

class Alters k a f where

  alterMany :: [k] -> (Map k a -> f (Map k a)) -> f (Map k a)
  default alterMany :: (Ord k, Monad f) => [k] -> (Map k a -> f (Map k a)) -> f (Map k a)
  alterMany ks f = do
    m <- lookupMany ks
    m' <- f m
    deleteMany @k @a . M.keys $ m M.\\ m'
    insertMany m'
    return m'

  alter :: k -> (Maybe a -> f (Maybe a)) -> f (Maybe a)
  default alter :: (Ord k, Functor f) => k -> (Maybe a -> f (Maybe a)) -> f (Maybe a)
  alter k f = M.lookup k <$> alterMany [k] (M.alterF f k)

  lookupMany :: [k] -> f (Map k a)
  default lookupMany :: (Ord k, Monad f) => [k] -> f (Map k a)
  lookupMany ks = M.fromList . catMaybes <$> forM ks (\k -> fmap (k,) <$> lookup k)

  lookup :: k -> f (Maybe a)
  default lookup :: Applicative f => k -> f (Maybe a)
  lookup k = alter k pure

  insertMany :: Map k a -> f ()
  default insertMany :: (Ord k, Monad f) => Map k a -> f ()
  insertMany m = forM_ (M.assocs m) . uncurry $ insert

  insert :: k -> a -> f ()
  default insert :: Applicative f => k -> a -> f ()
  insert k a = alter_ k (pure . const (Just a))

  deleteMany :: [k] -> f ()
  default deleteMany :: (Ord k, Monad f) => [k] -> f ()
  deleteMany ks = mapM_ (delete @k @a) ks

  delete :: k -> f ()
  default delete :: Applicative f => k -> f ()
  delete k = alter_ @k @a k (pure . const Nothing)

  {-# MINIMAL alterMany
            | alter
            | lookupMany, insertMany, deleteMany
            | lookup, insert, delete
    #-}

  alterMany_ :: [k] -> (Map k a -> f (Map k a)) -> f ()
  default alterMany_ :: Functor f => [k] -> (Map k a -> f (Map k a)) -> f ()
  alterMany_ ks = void . alterMany ks

  alter_ :: k -> (Maybe a -> f (Maybe a)) -> f ()
  default alter_ :: Functor f => k -> (Maybe a -> f (Maybe a)) -> f ()
  alter_ k = void . alter k

  lookupWithDefault :: k -> f a
  default lookupWithDefault :: (Default a, Functor f) => k -> f a
  lookupWithDefault k = fromMaybe def <$> lookup k

  lookupWithMempty :: k -> f a
  default lookupWithMempty :: (Monoid a, Functor f) => k -> f a
  lookupWithMempty k = fromMaybe mempty <$> lookup k

  update :: k -> (a -> f (Maybe a)) -> f (Maybe a)
  default update :: Applicative f => k -> (a -> f (Maybe a)) -> f (Maybe a)
  update k f = alter k $ \case
    Just a -> f a
    Nothing -> pure Nothing

  update_ :: k -> (a -> f (Maybe a)) -> f ()
  default update_ :: Functor f => k -> (a -> f (Maybe a)) -> f ()
  update_ k = void . update k

  updateStatefully :: k -> StateT a f (Maybe a) -> f (Maybe a)
  default updateStatefully :: Monad f => k -> StateT a f (Maybe a) -> f (Maybe a)
  updateStatefully k = update k . evalStateT

  updateStatefully_ :: k -> StateT a f (Maybe a) -> f ()
  default updateStatefully_ :: Monad f => k -> StateT a f (Maybe a) -> f ()
  updateStatefully_ k = void . updateStatefully k

  adjust :: k -> (a -> f a) -> f a
  default adjust :: Functor f => k -> (a -> f a) -> f a
  adjust k f = fmap fromJust $ update k (fmap Just . f)

  adjust_ :: k -> (a -> f a) -> f ()
  default adjust_ :: Functor f => k -> (a -> f a) -> f ()
  adjust_ k = void . adjust k

  adjustStatefully :: k -> StateT a f () -> f a
  default adjustStatefully :: Monad f => k -> StateT a f () -> f a
  adjustStatefully k = adjust k . execStateT

  adjustStatefully_ :: k -> StateT a f () -> f ()
  default adjustStatefully_ :: Monad f => k -> StateT a f () -> f ()
  adjustStatefully_ k = void . adjustStatefully k

  adjustWithDefault :: k -> (a -> f a) -> f a
  default adjustWithDefault :: (Default a, Functor f) => k -> (a -> f a) -> f a
  adjustWithDefault k f = fmap fromJust $ alter k (fmap Just . f . fromMaybe def)

  adjustWithDefault_ :: k -> (a -> f a) -> f ()
  default adjustWithDefault_ :: (Default a, Functor f) => k -> (a -> f a) -> f ()
  adjustWithDefault_ k = void . adjustWithDefault k

  adjustWithDefaultStatefully :: Default a => k -> StateT a f () -> f a
  default adjustWithDefaultStatefully :: (Default a, Monad f) => k -> StateT a f () -> f a
  adjustWithDefaultStatefully k = adjustWithDefault k . execStateT

  adjustWithDefaultStatefully_ :: Default a => k -> StateT a f () -> f ()
  default adjustWithDefaultStatefully_ :: (Default a, Monad f) => k -> StateT a f () -> f ()
  adjustWithDefaultStatefully_ k = void . adjustWithDefaultStatefully k

  adjustWithMempty :: k -> (a -> f a) -> f a
  default adjustWithMempty :: (Monoid a, Functor f) => k -> (a -> f a) -> f a
  adjustWithMempty k f = fmap fromJust $ alter k (fmap Just . f . fromMaybe mempty)

  adjustWithMempty_ :: Monoid a => k -> (a -> f a) -> f ()
  default adjustWithMempty_ :: (Monoid a, Functor f) => k -> (a -> f a) -> f ()
  adjustWithMempty_ k = void . adjustWithMempty k

  adjustWithMemptyStatefully :: Monoid a => k -> StateT a f () -> f a
  default adjustWithMemptyStatefully :: (Monoid a, Monad f) => k -> StateT a f () -> f a
  adjustWithMemptyStatefully k = adjustWithMempty k . execStateT

  adjustWithMemptyStatefully_ :: Monoid a => k -> StateT a f () -> f ()
  default adjustWithMemptyStatefully_ :: (Monoid a, Monad f) => k -> StateT a f () -> f ()
  adjustWithMemptyStatefully_ k = void . adjustWithMemptyStatefully k

  repsert :: k -> (Maybe a -> f a) -> f a
  default repsert :: Functor f => k -> (Maybe a -> f a) -> f a
  repsert k f = fmap fromJust $ alter k (fmap Just . f)

  repsert_ :: k -> (Maybe a -> f a) -> f ()
  default repsert_ :: Functor f => k -> (Maybe a -> f a) -> f ()
  repsert_ k = void . repsert k

  exists :: k -> f Bool
  default exists :: Functor f => k -> f Bool
  exists k = isJust <$> lookup @k @a k



class Maps k a b where
  that :: k -> Lens' b (Maybe a)

instance Ord k => (k `Maps` a) (Map k a) where
  that k = lens (M.lookup k) (flip (maybe (M.delete k) (M.insert k)))

instance (Int `Maps` a) (IM.IntMap a) where
  that k = lens (IM.lookup k) (flip (maybe (IM.delete k) (IM.insert k)))



class (Ord k, Monad f) => Selectable k a f where
  selectMany :: [k] -> f (Map k a)
  selectMany ks = M.fromList . catMaybes <$> forM ks (\k -> fmap (k,) <$> select k)

  select :: k -> f (Maybe a)
  select k = M.lookup k <$> selectMany [k]

  {-# MINIMAL selectMany | select #-}

  selectWithDefault :: (Default a, Functor f) => k -> f a
  selectWithDefault k = fromMaybe def <$> select k

  selectWithMempty :: (Monoid a, Functor f) => k -> f a
  selectWithMempty k = fromMaybe mempty <$> select k

class Replaceable k a f where
  replace :: k -> a -> f ()

class Removable k a f where
  remove :: k -> f ()
