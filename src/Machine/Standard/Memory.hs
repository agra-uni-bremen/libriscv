{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Machine.Standard.Memory where

import Conversion
import Control.Monad
import Common.Types
import Common.Utils ( fstWordLe )
import Data.Int ()
import Data.Bits ( Bits((.|.), (.&.), shift, shiftR) )
import Data.Word ( Word8, Word32 )
import Data.Array.IO
    ( IOArray, readArray, writeArray, MArray(getBounds, newArray_) )
import qualified Data.ByteString.Lazy as BSL

class Storable wordType byteType where
    toWord  :: [byteType] -> wordType
    toBytes :: wordType   -> [byteType]
    -- XXX: Also need to account for halfs (16-bit) later

instance Storable Word32 Word8 where
    toWord  = bytesToWord
    toBytes = wordToBytes

-- Converts a list of bytes to a Word32 in MSB.
bytesToWord :: [Word8] -> Word32
bytesToWord bytes = foldl (\x (byte,idx) -> (fromIntegral byte `shift` (idx * 8)) .|. x)
    0 (zip bytes $ reverse [0..(length bytes) - 1])

-- Split a 32-bit word into four octets.
wordToBytes :: Word32 -> [Word8]
wordToBytes w = map (\off -> fromIntegral $ shiftR w off .&. 0xff) offs
    where
        offs = reverse $ take 4 $ iterate (+8) 0

------------------------------------------------------------------------

-- Byte-addressable memory.
type Memory a = (Address, IOArray Address a)

-- Create a new memory of the given size starting at the given address.
mkMemory :: Address -> Word32 -> IO (Memory a)
mkMemory addr size = fmap (addr, ) (newArray_ (0, size - 1) :: IO (IOArray Address a))

-- Translate global address to a memory-local address.
toMemAddr :: Memory a -> Address -> Address
toMemAddr (startAddr, _) addr = addr - startAddr

-- Returns the size of the memory in bytes.
memSize :: Memory a -> IO Word32
memSize = fmap ((+1) . snd) .  getBounds . snd

------------------------------------------------------------------------

loadByte :: Memory a -> Address -> IO a
loadByte mem@(_, array) = readArray array . toMemAddr mem

loadWord :: Storable b a => Memory a -> Address -> IO b
loadWord mem addr = toWord <$>
    mapM (\off -> loadByte mem $ addr + off) [0..3]

-- Store a byte at the given address in memory.
storeByte :: Memory a -> Address -> a -> IO ()
storeByte mem@(_, array) addr = writeArray array $ toMemAddr mem addr

-- Store a word at the given address in memory.
storeWord :: Storable b a => Memory a -> Address -> b -> IO ()
storeWord mem addr =
    mapM_ (\(off, val) -> storeByte mem (addr + off) val)
        . zip [0..] . toBytes

-- Write a ByteString to memory in little endian byteorder.
storeByteString :: (Conversion Word8 a) => Memory a -> Address -> BSL.ByteString -> IO ()
storeByteString mem addr bs =
    mapM_ (\(off, val) -> storeByte mem (addr + off) (convert val))
        $ zip [0..] (reverse $ BSL.unpack bs)
