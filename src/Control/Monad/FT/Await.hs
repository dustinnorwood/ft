{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Monad.FT.Await
  ( Awaits(..)
  ) where

import Control.Monad ((<=<))
import Data.Foldable (traverse_)

{- The Awaits Typeclass
  (f `Awaits` a) is a typeclass used to generalize the `await` function from streaming
  libraries like Pipes and Conduit to any monad f.
  The class has two type parameters:
    f - the underlying monad, such as `ConduitT i o m r`
    a - the value type being awaited, like the `i` in `ConduitT i o m r`
-}
class Awaits a f where
  await :: f (Maybe a)

  {-# MINIMAL await #-}

  awaitForever :: (a -> f ()) -> f ()
  default awaitForever :: Monad f => (a -> f ()) -> f ()
  awaitForever f = go
    where go = await >>= traverse_ (const go <=< f)