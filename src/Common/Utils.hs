module Common.Utils
(
    fstWordLe
    , whenMword
    , unlessMword
)
where

import Data.Word ( Word8, Word32 )
import Data.Bits ( Bits((.|.), shiftR, (.&.), shift) )
import qualified Data.ByteString.Lazy as BSL
import Control.Monad (when,unless)

-- Read the first 32-bit word in little endian from a bytestring.
fstWordLe :: BSL.ByteString -> Word32
fstWordLe b = fromIntegral (BSL.index b 0)
    .|. (fromIntegral (BSL.index b 1) `shift` 8)
    .|. (fromIntegral (BSL.index b 2) `shift` 16)
    .|. (fromIntegral (BSL.index b 3) `shift` 24)

-- Check if a Word32 represents a true value.
wordPred :: Word32 -> Bool
wordPred = (==) 1

whenMword :: Monad m => m Word32 -> m () -> m()
whenMword mb act = mb >>= (pure . wordPred) >>= flip when act

unlessMword :: Monad m => m Word32 -> m () -> m()
unlessMword mb act = mb >>= (pure . wordPred) >>= flip unless act
