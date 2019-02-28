{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Control.Comonad.Change.Alter
  ( CoAlters(..)
  ) where

import Control.Comonad
import Data.Proxy

class Comonad w => CoAlters k a w where
  coalter :: Proxy a -> k -> (w (Maybe a) -> Maybe a) -> Maybe a

-- TODO: Fill this in
{-
coalter_ :: (k `CoAlters` a) w => Proxy a -> k -> (w (Maybe a) -> Maybe a) -> ()
coalter_ p k = const () $ coalter p k

coupdate :: (k `CoAlters` a) w => Proxy a -> k -> (w a -> Maybe a) -> w (Maybe a)
coupdate p k f = coalter p k $ \wma -> case cosequence wma of
  Just wa -> coalter p k (cosequence $ Just wa)
  Nothing -> wma

coupdate_ :: (k `CoAlters` a) w => Proxy a -> k -> (a -> w (Maybe a)) -> ()
coupdate_ p k = const () $ coupdate p k

coupdateStorefully :: (k `CoAlters` a) w => Proxy a -> k -> StoreT a w (Maybe a) -> Maybe a
coupdateStorefully p k = coupdate p k . peek

coupdateStorefully_ :: (k `CoAlters` a) w => Proxy a -> k -> StoreT a w (Maybe a) -> ()
coupdateStorefully_ p k = const () $ coupdateStorefully p k

coadjust :: (k `CoAlters` a) w => Proxy a -> k -> (w a -> a) -> a
coadjust p k w = fromJust $ coupdate p k (Just . f)

coadjust_ :: (k `CoAlters` a) w => Proxy a -> k -> (w a -> a) -> ()
coadjust_ p k = const () $ coadjust p k

coadjustStatefully :: (k `CoAlters` a) w => Proxy a -> k -> StoreT a w () -> a
coadjustStatefully p k = coadjust p k . pos

coadjustStorefully_ :: (k `CoAlters` a) w => Proxy a -> k -> StoreT a w () -> ()
coadjustStorefully_ p k = const () $ coadjustStorefully p k

corepsert :: (k `CoAlters` a) w => Proxy a -> k -> (w (Maybe a) -> a) -> a
corepsert p k w = fromJust $ coalter p k (Just . f)

corepsert_ :: (k `CoAlters` a) w => Proxy a -> k -> (w (Maybe a) -> a) -> ()
corepsert_ p k = const () $ corepsert p k

colookup :: (k `CoAlters` a) w => Proxy a -> k -> Maybe a
colookup p k = coalter p k extract

coinsert :: (k `CoAlters` a) w => Proxy a -> k -> a -> ()
coinsert p k a = corepsert_ p k (const a)

codelete :: (k `CoAlters` a) w => Proxy a -> k -> w ()
codelete p k = coalter_ p k (const Nothing)
-}
