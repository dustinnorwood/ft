{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Monad.FT.Input
  ( Inputs(..)
  , inputs
  ) where

class Inputs a f where
  input :: f a

inputs :: (Functor f, Inputs a f) => (a -> b) -> f b
inputs f = f <$> input