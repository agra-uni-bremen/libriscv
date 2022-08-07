module Utils
(
    getBytes
    , fstWord
)
where

import Data.Word
import Data.Bits
import qualified Data.ByteString.Lazy as BSL

-- | Split a 32-bit word into four octets.
--
-- Examples:
--
-- >>> getBytes 0xdeadbeef
-- [222,173,190,239]
--
getBytes :: Word32 -> [Word8]
getBytes w = map (\off -> fromIntegral $ (shiftR w off) .&. 0xff) offs
    where
        offs = reverse $ take 4 $ iterate (+8) 0

-- | Read the first 32-bit word from a bytestring.
--
-- Examples:
--
-- >>> fstWord $ BSL.pack [0x23, 0x42, 0x13, 0x37]
-- 924008995
fstWord :: BSL.ByteString -> Word32
fstWord b = (fromIntegral $ BSL.index b 0)
    .|. ((fromIntegral $ BSL.index b 1) `shift` 8)
    .|. ((fromIntegral $ BSL.index b 2) `shift` 16)
    .|. ((fromIntegral $ BSL.index b 3) `shift` 24)
