{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Monad.Change.Output
  ( Outputs(..)
  , genericOutputsStringIO
  ) where

import           Control.Lens
import           Control.Monad                    (void, mapM_)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State        (execStateT, runStateT, StateT)

class Outputs a f where
  output :: a -> f ()

genericOutputsStringIO :: MonadIO m => String -> m ()
genericOutputsStringIO = liftIO . putStrLn