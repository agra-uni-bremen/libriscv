{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Machine.Standard.Memory where

import Control.Monad
import Common.Types
import Common.Utils ( fstWordLe, getBytes )
import Data.Int ()
import Data.Bits ( Bits((.|.), shift) )
import Data.Word ( Word8, Word32 )
import Data.Array.IO
    ( IOUArray, readArray, writeArray, MArray(getBounds, newArray_) )
import qualified Data.ByteString.Lazy as BSL

-- Byte-addressable memory.
type Memory = (Address, IOUArray Address Word8)

-- Create a new memory of the given size starting at the given address.
mkMemory :: Address -> Word32 -> IO Memory
mkMemory addr size = fmap (addr, ) (newArray_ (0, size - 1) :: IO (IOUArray Address Word8))

-- Translate global address to a memory-local address.
toMemAddr :: Memory -> Address -> Address
toMemAddr (startAddr, _) addr = addr - startAddr

-- Returns the size of the memory in bytes.
memSize :: Memory -> IO Word32
memSize = fmap ((+1) . snd) .  getBounds . snd

-- Converts a list of bytes to a Word32 in MSB.
bytesToWord :: [Word8] -> Word32
bytesToWord bytes = foldl (\x (byte,idx) -> (fromIntegral byte `shift` (idx * 8)) .|. x)
    0 (zip bytes $ reverse [0..(length bytes) - 1])

------------------------------------------------------------------------

loadByte :: Memory -> Address -> IO Word8
loadByte = readArray . snd

loadWord :: Memory -> Address -> IO Word32
loadWord mem addr = do
    bytes <- mapM (\off -> loadByte mem $ toMemAddr mem (addr + off)) [0..3]
    pure $ bytesToWord bytes

-- Store a byte at the given address in memory.
storeByte :: Memory -> Address -> Word8 -> IO ()
storeByte mem@(_, array) addr = writeArray array $ toMemAddr mem addr

-- Store a word at the given address in memory.
storeWord :: Memory -> Address -> Word32 -> IO ()
storeWord mem addr =
    mapM_ (\(off, val) -> storeByte mem (addr + off) val)
        . zip [(0 :: Address)..4] . getBytes

-- Write a ByteString to memory in little endian byteorder.
storeByteString :: Memory -> Address -> BSL.ByteString -> IO ()
storeByteString mem addr bs =
    mapM_ (\(off, val) -> storeWord mem (addr + off) $ fstWordLe val)
        $ zip [0,4..(fromIntegral $ BSL.length bs)] lst
    where
        lst = takeWhile (not . BSL.null) $ iterate (BSL.drop 4) bs
