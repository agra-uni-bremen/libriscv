{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
module LibRISCV.Utils where

import LibRISCV

import Data.Word ( Word32 )
import Control.Monad (when,unless)
import Control.Monad.Freer (type (~>))
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))

-- Convert bool to a Word32.
boolToWord :: Bool -> Word32
boolToWord True  = 1
boolToWord False = 0

-- Like Control.Monad.when but interprets given Word32 as a bool.
whenMword :: Monad m => m Word32 -> m () -> m ()
whenMword mb act = mb >>= flip when act . (==1)

-- Like Control.Monad.unless but interprets given Word32 as a bool.
unlessMword :: Monad m => m Word32 -> m () -> m ()
unlessMword mb act = mb >>= flip unless act . (==1)

------------------------------------------------------------------------

-- Align an address on the next word boundary.
align :: Address -> Address
align addr = addr - addr `mod` 4

extends :: Monad m => (e -> f w ~> MaybeT m) -> (e -> f w ~> m) -> (e -> f w ~> m)  
extends ext def env inst = runMaybeT (ext env inst) >>= \case
    Just x -> pure x
    Nothing -> def env inst
