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

-- Read the first 32-bit word in little endian from a bytestring.
fstWordLe :: BSL.ByteString -> Word32
fstWordLe b = fromIntegral (BSL.index b 0)
    .|. (fromIntegral (BSL.index b 1) `shift` 8)
    .|. (fromIntegral (BSL.index b 2) `shift` 16)
    .|. (fromIntegral (BSL.index b 3) `shift` 24)


whenMword :: Monad m => m Word32 -> m () -> m ()
whenMword mb act = mb >>= flip when act . (==1)

unlessMword :: Monad m => m Word32 -> m () -> m ()
unlessMword mb act = mb >>= flip unless act . (==1)

extends :: Monad m => (e -> f w ~> MaybeT m) -> (e -> f w ~> m) -> (e -> f w ~> m)  
extends ext def env inst = runMaybeT (ext env inst) >>= \case
    Just x -> pure x
    Nothing -> def env inst