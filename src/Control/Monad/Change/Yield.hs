{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Monad.Change.Yield
  ( Yields(..)
  ) where

{- The Yields Typeclass
  (f `Yields` a) is a typeclass used to generalize the `yield` function from streaming
  libraries like Pipes and Conduit to any monad f.
  The class has two type parameters:
    f - the underlying monad, such as `ConduitT i o m r`
    a - the value type being awaited, like the `o` in `ConduitT i o m r`
-}
class Yields f a where
  yield :: a -> f ()
  default yield :: Monad f => a -> f ()
  yield a = yieldMany [a]

  yieldMany :: [a] -> f ()
  default yieldMany :: Monad f => [a] -> f ()
  yieldMany = mapM_ yield

  {-# MINIMAL yield | yieldMany #-}
