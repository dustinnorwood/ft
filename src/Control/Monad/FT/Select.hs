{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
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

module Control.Monad.FT.Select
  ( Selectable(..)
  , Selects
  ) where

import           Control.Monad
import           Data.Default
import           Data.Maybe

{- The Selectable Typeclass
  `Selectable k a f` is a typeclass used to generalize the Data.Map function `select` to any monad f.
  The class has three type parameters:
    a - the value type, like the `a` in `Map k a`
    k - the key type, like the `k` in `Map k a`
    f - the underlying monad, such as `State (Map k a)`
-}
class Selectable a k f where
  {- select
     Select the corresponding value for a given key `k` in the underlying monad `f`
     This function is analogous to the `select` function in `Alterable`
  -}
  select :: k -> f (Maybe a)
  default select :: (Ord k, Functor f) => k -> f (Maybe a)
  select k = join . listToMaybe . map snd <$> selectMany [k]

  {- selectMany
     Take a list of keys, and return a `[(k, Maybe a)]` of the keys and their values existing in the
     underlying type constructor `f`.
  -}
  selectMany :: [k] -> f [(k, Maybe a)]
  default selectMany :: (Ord k, Applicative f) => [k] -> f [(k, Maybe a)]
  selectMany = traverse (\k -> (k,) <$> select k)

  {-# MINIMAL select | selectMany #-}

  {- selectWithDefault
     Select the corresponding value for a given key `k` in the underlying monad `f`,
     and return `def` if the entry for key `k` is not found.
     Requires a `Default` instance on the value type `a`.
     This function is analogous to the `selectWithDefault` function in `Alterable`
  -}
  selectWithDefault :: k -> f a
  default selectWithDefault :: (Default a, Functor f) => k -> f a
  selectWithDefault k = fromMaybe def <$> select k

  {- selectWithMempty
     Select the corresponding value for a given key `k` in the underlying monad `f`,
     and return `mempty` if the entry for key `k` is not found.
     Requires a `Monoid` instance on the value type `a`.
     This function is analogous to the `selectWithMempty` function in `Alterable`
  -}
  selectWithMempty :: k -> f a
  default selectWithMempty :: (Monoid a, Functor f) => k -> f a
  selectWithMempty k = fromMaybe mempty <$> select k

  {- exists
     Returns a Bool representing whether the entry for a given key `k` exists in the
     underlying monad `f`. Although this is trivially implemented as `isJust <$> select p k`,
     in cases where the backend can make this determination without returning the value itself,
     utilizing this capability can be substantially more performant for large values.
  -}
  exists :: k -> f Bool
  default exists :: Functor f => k -> f Bool
  exists k = isJust <$> select @a k

type Selects k a = Selectable a k