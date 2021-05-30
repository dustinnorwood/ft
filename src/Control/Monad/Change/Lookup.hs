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

module Control.Monad.Change.Lookup
  ( Lookupable(..)
  , Lookups
  ) where

import           Control.Monad
import           Data.Default
import           Data.Maybe
import           Prelude                     hiding (lookup)

{- The Lookupable Typeclass
  `Lookupable k a f` is a typeclass used to generalize the Data.Map function `lookup` to any monad f.
  The class has three type parameters:
    a - the value type, like the `a` in `Map k a`
    k - the key type, like the `k` in `Map k a`
    f - the underlying monad, such as `State (Map k a)`
-}
class Lookupable a k f where
  {- lookup
     Lookup the corresponding value for a given key `k` in the underlying monad `f`
     This function is analogous to the `lookup` function in `Alterable`
  -}
  lookup :: k -> f (Maybe a)
  default lookup :: (Ord k, Functor f) => k -> f (Maybe a)
  lookup k = join . listToMaybe . map snd <$> lookupMany [k]

  {- lookupMany
     Take a list of keys, and return a `[(k, Maybe a)]` of the keys and their values existing in the
     underlying type constructor `f`.
  -}
  lookupMany :: [k] -> f [(k, Maybe a)]
  default lookupMany :: (Ord k, Applicative f) => [k] -> f [(k, Maybe a)]
  lookupMany = traverse (\k -> (k,) <$> lookup k)

  {-# MINIMAL lookup | lookupMany #-}

  {- lookupWithDefault
     Lookup the corresponding value for a given key `k` in the underlying monad `f`,
     and return `def` if the entry for key `k` is not found.
     Requires a `Default` instance on the value type `a`.
     This function is analogous to the `lookupWithDefault` function in `Alterable`
  -}
  lookupWithDefault :: k -> f a
  default lookupWithDefault :: (Default a, Functor f) => k -> f a
  lookupWithDefault k = fromMaybe def <$> lookup k

  {- lookupWithMempty
     Lookup the corresponding value for a given key `k` in the underlying monad `f`,
     and return `mempty` if the entry for key `k` is not found.
     Requires a `Monoid` instance on the value type `a`.
     This function is analogous to the `lookupWithMempty` function in `Alterable`
  -}
  lookupWithMempty :: k -> f a
  default lookupWithMempty :: (Monoid a, Functor f) => k -> f a
  lookupWithMempty k = fromMaybe mempty <$> lookup k

  {- exists
     Returns a Bool representing whether the entry for a given key `k` exists in the
     underlying monad `f`. Although this is trivially implemented as `isJust <$> lookup p k`,
     in cases where the backend can make this determination without returning the value itself,
     utilizing this capability can be substantially more performant for large values.
  -}
  exists :: k -> f Bool
  default exists :: Functor f => k -> f Bool
  exists k = isJust <$> lookup @a k

type Lookups k a = Lookupable a k