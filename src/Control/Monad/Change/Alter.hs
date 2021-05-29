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

module Control.Monad.Change.Alter
  ( Alterable(..)
  , Alters
  ) where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Change.Delete
import           Control.Monad.Change.Insert
import           Control.Monad.Change.Lookup
import           Control.Monad.Trans.State   (evalStateT, execStateT, StateT)
import           Data.Default
import           Data.Foldable               (traverse_)
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as M
import           Data.Maybe
import           Prelude                     hiding (lookup)

{- The Alterable Typeclass
  Alterable a k f is a typeclass used to generalize Data.Map-like functions to any type constructor f.
  The class has three type parameters:
    a - the value type, like the `a` in `Map k a`
    k - the key type, like the `k` in `Map k a`
    f - the underlying monad, such as `State (Map k a)`
  Alterable a k f allows reusable code to be written for different contexts. For example,
  instances can be written for production monads, using calls to external databases;
  testing monads, where the Map-like structure is held in-memory;
  or hybrids, where the user wishes to implement a sophisticated caching system in-memory,
  and have fallback calls to the database. Using `Alters`, the memory management logic is
  separated from the backend-agnostic business logic.

  The Alterable typeclass contains two basic sets of functions to represent Map-like operations:
  `lookup`, `insert`, and `delete`; and `alter`. Instances of Alterable may choose to implement
  one or both of these sets, depending on the details of the underlying monad. For example,
  if connecting to a simple key-value store, which only supports lookup, insert, and delete,
  it makes sense to only implement those respective functions in the monad's `Alterable` instance,
  because there is no way to leverage the database's API to optimize more complex functions.
  In this case, the defaults for composite functions like `alter`, `adjust`, and `update` are
  implemented using combinations of `lookup`, `insert`, and `delete`. However, let's say the
  database we're connecting to supports an update function, which can be called without having
  to retrieve and replace the value associated with a certain key, then it would be more
  efficient to separately implement the `update` or `adjust` functions in the monad's `Alterable`
  instance, rather than using the default, which is the composition of `lookup` and `insert`.
  Now, let's say the backend supports some more advanced query language, such as SQL, which
  supports complex CRUD operations, as well as functions to modify data in-place. In this case,
  it might make more sense to implement `alter` and/or `alterMany`, instead of, or in addition to,
  the standard `lookup`, `insert`, and `delete` functions, because `alterMany` may be written as
  a SQL query to update millions of rows in a table, without the Haskell code ever having to see
  that data. This is why all of the functions in the Alterable typeclass are inside the class, rather
  than top-level functions with an `Alterable` constraint.
-}
class ( Lookupable a k f
      , Insertable a k f
      , Deletable  a k f
      )
     => Alterable  a k f where

  {- alter
     The most general function that can be applied to a Map-like structure.
     Apply an effectful function to a `Maybe a`, which represents a value that may or may not
     exist in a `Map k a`, at a given key `k`, and return a `Maybe a`, which represents whether
     to insert the new value in the Map. From `alter`, we can derive every other function in the
     `Alterable` typeclass. However, for many cases, defining `alter` by itself is not the most
     efficient implementation for the underlying monad. The default instance is implemented as
     a combination of `lookup`, `insert`, and `delete`, but could also be implemented as
     the singleton case of `alterMany`.
     NB: Since most monads implement `Alterable` only using `lookup`, `insert`, and `delete`,
         and all of the compound functions are defined in terms of `alter`, it only makes
         sense to define `alter` in terms of `lookup`, `insert`, and `delete`.
  -}
  alter :: k -> (Maybe a -> f (Maybe a)) -> f (Maybe a)
  default alter :: (Monad f) => k -> (Maybe a -> f (Maybe a)) -> f (Maybe a)
  alter k f = do
    ma <- lookup k
    ma' <- f ma
    case ma' of
      Just a -> insert k a
      Nothing -> when (isJust ma) $ delete @a k
    return ma'

  {- alterMany
     Apply an effectful function to a `Map k a`, and return the new Map.
     The default instance is a combination of `{lookup, insert, delete}Many`.
  -}
  alterMany :: [k] -> (Map k a -> f (Map k a)) -> f (Map k a)
  default alterMany :: (Ord k, Monad f) => [k] -> (Map k a -> f (Map k a)) -> f (Map k a)
  alterMany ks f = do
    m <- lookupMany ks
    m' <- f m
    deleteMany @a . M.keys $ m M.\\ m'
    insertMany m'
    return m'

  {- alter_
     Same as `alter` except it discards the return value.
  -}
  alter_ :: k -> (Maybe a -> f (Maybe a)) -> f ()
  default alter_ :: Functor f => k -> (Maybe a -> f (Maybe a)) -> f ()
  alter_ k = void . alter k

  {- alterMany_
     Same as `alterMany` except it discards the return value.
  -}
  alterMany_ :: [k] -> (Map k a -> f (Map k a)) -> f ()
  default alterMany_ :: Functor f => [k] -> (Map k a -> f (Map k a)) -> f ()
  alterMany_ ks = void . alterMany ks

  {- update
     Apply an effectful function on an existing key/value pair in the underlying monad `f`.
     If the entry for key `k` does not exist in the underlying monad beforehand, the function
     is not applied. If the function returns `Nothing`, the entry for key `k` is deleted from
     the underlying monad.
  -}
  update :: k -> (a -> f (Maybe a)) -> f (Maybe a)
  default update :: Applicative f => k -> (a -> f (Maybe a)) -> f (Maybe a)
  update k f = alter k $ \case
    Just a -> f a
    Nothing -> pure Nothing

  {- update_
     Same as `update` except it discards the return value.
  -}
  update_ :: k -> (a -> f (Maybe a)) -> f ()
  default update_ :: Functor f => k -> (a -> f (Maybe a)) -> f ()
  update_ k = void . update k

  {- updateStatefully
     Same as `update`, but run in a stateful context.
     This is useful when applying complex functions to the value, especially when the using
     lenses to operate on specific fields in the record type.
  -}
  updateStatefully :: k -> StateT a f (Maybe a) -> f (Maybe a)
  default updateStatefully :: Monad f => k -> StateT a f (Maybe a) -> f (Maybe a)
  updateStatefully k = update k . evalStateT

  {- updateStatefully_
     Same as `updateStatefully` except it discards the return value.
  -}
  updateStatefully_ :: k -> StateT a f (Maybe a) -> f ()
  default updateStatefully_ :: Functor f => k -> StateT a f (Maybe a) -> f ()
  updateStatefully_ k = void . updateStatefully k

  {- adjust
     Apply an effectful function on an existing key/value pair in the underlying monad `f`.
     If the entry for key `k` does not exist in the underlying monad beforehand, the function
     is not applied. Unlike `update`, `adjust` returns an `a` instead of a `Maybe a`, which
     means that the entry for key `k` cannot be deleted from the underlying monad by calling
     `adjust`.
  -}
  adjust :: k -> (a -> f a) -> f a
  default adjust :: Functor f => k -> (a -> f a) -> f a
  adjust k f = fromJust <$> update k (fmap Just . f)

  {- adjust_
     Same as `adjust` except it discards the return value.
  -}
  adjust_ :: k -> (a -> f a) -> f ()
  default adjust_ :: Functor f => k -> (a -> f a) -> f ()
  adjust_ k = void . adjust k

  {- adjustStatefully
     Same as `adjust`, but run in a stateful context.
     This is useful when applying complex functions to the value, especially when the using
     lenses to operate on specific fields in the record type.
  -}
  adjustStatefully :: k -> StateT a f () -> f a
  default adjustStatefully :: Monad f => k -> StateT a f () -> f a
  adjustStatefully k = adjust k . execStateT

  {- adjustStatefully_
     Same as `adjustStatefully` except it discards the return value.
  -}
  adjustStatefully_ :: k -> StateT a f () -> f ()
  default adjustStatefully_ :: Functor f => k -> StateT a f () -> f ()
  adjustStatefully_ k = void . adjustStatefully k

  {- adjustWithDefault
     Adjust the corresponding value for a given key `k` in the underlying monad `f`,
     If the entry for key `k` does not exist in the underlying monad beforehand, `def`
     is passed to the function. This ensures that the entry for key `k` will exist
     in the underlying monad after calling `adjustWithDefault`.
     Requires a `Default` instance on the value type `a`.
  -}
  adjustWithDefault :: k -> (a -> f a) -> f a
  default adjustWithDefault :: (Default a, Functor f) => k -> (a -> f a) -> f a
  adjustWithDefault k f = fromJust <$> alter k (fmap Just . f . fromMaybe def)

  {- adjustWithDefault_
     Same as `adjustWithDefault` except it discards the return value.
     Requires a `Default` instance on the value type `a`.
  -}
  adjustWithDefault_ :: k -> (a -> f a) -> f ()
  default adjustWithDefault_ :: Functor f => k -> (a -> f a) -> f ()
  adjustWithDefault_ k = void . adjustWithDefault k

  {- adjustWithDefaultStatefully
     Same as `adjustWithDefault`, but run in a stateful context.
     This is useful when applying complex functions to the value, especially when the using
     lenses to operate on specific fields in the record type.
     Requires a `Default` instance on the value type `a`.
  -}
  adjustWithDefaultStatefully :: k -> StateT a f () -> f a
  default adjustWithDefaultStatefully :: Monad f => k -> StateT a f () -> f a
  adjustWithDefaultStatefully k = adjustWithDefault k . execStateT

  {- adjustWithDefaultStatefully_
     Same as `adjustWithDefaultStatefully` except it discards the return value.
     Requires a `Default` instance on the value type `a`.
  -}
  adjustWithDefaultStatefully_ :: k -> StateT a f () -> f ()
  default adjustWithDefaultStatefully_ :: Functor f => k -> StateT a f () -> f ()
  adjustWithDefaultStatefully_ k = void . adjustWithDefaultStatefully k

  {- adjustWithMempty
     Adjust the corresponding value for a given key `k` in the underlying monad `f`,
     If the entry for key `k` does not exist in the underlying monad beforehand, `mempty`
     is passed to the function. This ensures that the entry for key `k` will exist
     in the underlying monad after calling `adjustWithMempty`.
     Requires a `Monoid` instance on the value type `a`.
  -}
  adjustWithMempty :: k -> (a -> f a) -> f a
  default adjustWithMempty :: (Monoid a, Functor f) => k -> (a -> f a) -> f a
  adjustWithMempty k f = fromJust <$> alter k (fmap Just . f . fromMaybe mempty)

  {- adjustWithMempty_
     Same as `adjustWithMempty` except it discards the return value.
     Requires a `Monoid` instance on the value type `a`.
  -}
  adjustWithMempty_ :: k -> (a -> f a) -> f ()
  default adjustWithMempty_ :: Functor f => k -> (a -> f a) -> f ()
  adjustWithMempty_ k = void . adjustWithMempty k

  {- adjustWithMemptyStatefully
     Same as `adjustWithMempty`, but run in a stateful context.
     This is useful when applying complex functions to the value, especially when the using
     lenses to operate on specific fields in the record type.
     Requires a `Monoid` instance on the value type `a`.
  -}
  adjustWithMemptyStatefully :: k -> StateT a f () -> f a
  default adjustWithMemptyStatefully :: Monad f => k -> StateT a f () -> f a
  adjustWithMemptyStatefully k = adjustWithMempty k . execStateT

  {- adjustWithMemptyStatefully_
     Same as `adjustWithMemptyStatefully` except it discards the return value.
     Requires a `Monoid` instance on the value type `a`.
  -}
  adjustWithMemptyStatefully_ :: Monoid a => k -> StateT a f () -> f ()
  default adjustWithMemptyStatefully_ :: Functor f => k -> StateT a f () -> f ()
  adjustWithMemptyStatefully_ k = void . adjustWithMemptyStatefully k

  {- repsert
     Insert a key/value pair into the underlying monad `f`, but use the existing value,
     whether it exists or not, as a parameter to an effectful function to determine
     the new value. This is a generalization of `adjustWith{Default,Mempty}`, where a custom
     value may be supplied if the entry for key `k` doesn't already exist in the underlying
     monad `f`.
  -}
  repsert :: k -> (Maybe a -> f a) -> f a
  default repsert :: Functor f => k -> (Maybe a -> f a) -> f a
  repsert k f = fromJust <$> alter k (fmap Just . f)

  {- repsert_
     Same as `repsert` except it discards the return value.
  -}
  repsert_ :: k -> (Maybe a -> f a) -> f ()
  default repsert_ :: Functor f => k -> (Maybe a -> f a) -> f ()
  repsert_ k = void . repsert k

type Alters k a = Alterable a k