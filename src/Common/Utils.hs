module Common.Utils
(
    getBytes
    , fstWordLe
    , whenMword
)
where

import Data.Word ( Word8, Word32 )
import Data.Bits ( Bits((.|.), shiftR, (.&.), shift) )
import qualified Data.ByteString.Lazy as BSL
import Control.Monad (when)

-- Split a 32-bit word into four octets.
getBytes :: Word32 -> [Word8]
getBytes w = map (\off -> fromIntegral $ shiftR w off .&. 0xff) offs
    where
        offs = reverse $ take 4 $ iterate (+8) 0

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
