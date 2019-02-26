module Control.Monad.Change
  ( X(..)
  , Modifiable(..)
  , Alters(..)
  ) where

import Control.Monad.Change.Alter
import Control.Monad.Change.Modify
import Control.Monad.Change.Proxy
import Prelude                     hiding (lookup)
