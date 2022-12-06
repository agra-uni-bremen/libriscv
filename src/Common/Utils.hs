{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
module Common.Utils where

import Data.Word ( Word8, Word32 )
import Data.Bits ( Bits((.|.), shiftR, (.&.), shift) )
import qualified Data.ByteString.Lazy as BSL
import Control.Monad (when,unless)
import Spec.Instruction (Instruction)
import Control.Monad.Freer (type (~>))
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))

-- Like Control.Monad.when but interprets given Word32 as a bool.
whenMword :: Monad m => m Word32 -> m () -> m ()
whenMword mb act = mb >>= flip when act . (==1)

-- Like Control.Monad.unless but interprets given Word32 as a bool.
unlessMword :: Monad m => m Word32 -> m () -> m ()
unlessMword mb act = mb >>= flip unless act . (==1)

extends :: Monad m => (e -> f w ~> MaybeT m) -> (e -> f w ~> m) -> (e -> f w ~> m)  
extends ext def env inst = runMaybeT (ext env inst) >>= \case
    Just x -> pure x
    Nothing -> def env inst
