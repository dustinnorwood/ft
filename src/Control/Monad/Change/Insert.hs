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

module Control.Monad.Change.Insert
  ( Insertable(..)
  , Inserts
  ) where

import           Data.Foldable               (traverse_)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as M
import           Data.Maybe

{- The Insertable Typeclass
  `Insertable k a f` is a typeclass used to generalize the Data.Map function `insert` to any monad f.
  The class has three type parameters:
    k - the key type, like the `k` in `Map k a`
    a - the value type, like the `a` in `Map k a`
    f - the underlying monad, such as `State (Map k a)`
  Use this typeclass instead of `Alterable` when `insert` functionality is all that is needed.
  NB: This typeclass may not be useful in real-world applications, but is provided for completeness.
-}
class Insertable a k f where
  {- insert
     Insert the corresponding key/value pair in the underlying monad `f`
  -}
  insert :: k -> a -> f ()
  default insert :: Applicative f => k -> a -> f ()
  insert k a = insertMany $ M.singleton k a

  {- insertMany
     Take a `Map k a`, and insert/overwrite its entries in the underlying monad `f`.
     The default instance is implemented as the list version of `insert`.
  -}
  insertMany :: Map k a -> f ()
  default insertMany :: Applicative f => Map k a -> f ()
  insertMany = traverse_ (uncurry insert) . M.assocs
  
  {-# MINIMAL insert | insertMany #-}

type Inserts k a = Insertable a k