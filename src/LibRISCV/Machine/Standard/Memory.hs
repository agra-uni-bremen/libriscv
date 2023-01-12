{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module LibRISCV.Machine.Standard.Memory where

import LibRISCV
import Conversion
import Data.Int ()
import Data.Bits ( Bits((.|.), (.&.), shift, shiftR) )
import Data.Word ( Word8, Word16, Word32 )
import Data.Array.IO
    ( readArray, writeArray, MArray(getBounds, newArray_) )
import qualified Data.ByteString.Lazy as BSL

-- Convert half to bytes (and vice versa) in little endian.
class HalfStorage halfType byteType where
    toHalf :: [byteType] -> halfType
    halfToBytes :: halfType -> [byteType]

-- Convert word to bytes (and vice versa) in little endian.
class WordStorage wordType byteType where
    toWord :: [byteType] -> wordType
    wordToBytes :: wordType -> [byteType]

instance WordStorage Word32 Word8 where
    toWord  = mkWord
    wordToBytes = mkBytes

instance HalfStorage Word16 Word8 where
    toHalf = fromIntegral . mkWord
    halfToBytes = mkBytes . fromIntegral

-- Converts a list of bytes to a Word32 in little endian.
mkWord :: [Word8] -> Word32
mkWord bytes = foldl (\x (byte,idx) -> (fromIntegral byte `shift` (idx * 8)) .|. x)
    0 $ zip bytes [0..(length bytes) - 1]

-- Split a 32-bit word into four octets in little endian.
mkBytes :: Word32 -> [Word8]
mkBytes w = map (\off -> fromIntegral $ shiftR w off .&. 0xff) offs
    where
        offs = take 4 $ iterate (+8) 0

------------------------------------------------------------------------

-- Byte-addressable memory.
type Memory t a = (Address, t Address a)

-- Create a new memory of the given size starting at the given address.
mkMemory :: MArray t a IO => Address -> Word32 -> IO (Memory t a)
mkMemory addr size = fmap (addr, ) (newArray_ (0, size - 1))

-- Translate global address to a memory-local address.
toMemAddr :: Memory t a -> Address -> Address
toMemAddr (startAddr, _) addr = addr - startAddr

-- Returns the size of the memory in bytes.
memSize :: MArray t a IO => Memory t a -> IO Word32
memSize = fmap ((+1) . snd) .  getBounds . snd

------------------------------------------------------------------------

loadByte :: MArray t a IO => Memory t a -> Address -> IO a
loadByte mem@(_, array) = readArray array . toMemAddr mem

load :: (MArray t a IO) => ([a] -> b) -> Word32 -> Memory t a -> Address -> IO b
load proc bytesize mem addr = proc <$>
    mapM (\off -> loadByte mem $ addr + off) [0..(bytesize - 1)]

loadHalf :: (MArray t a IO, HalfStorage b a) => Memory t a -> Address -> IO b
loadHalf = load toHalf 2

loadWord :: (MArray t a IO, WordStorage b a) => Memory t a -> Address -> IO b
loadWord = load toWord 4

storeByte :: MArray t a IO => Memory t a -> Address -> a -> IO ()
storeByte mem@(_, array) addr = writeArray array $ toMemAddr mem addr

store :: (MArray t a IO) => (b -> [a]) -> Word32 -> Memory t a -> Address -> b -> IO ()
store proc bytesize mem addr =
    mapM_ (\(off, val) -> storeByte mem (addr + off) val)
        . zip [0..(bytesize - 1)] . proc

storeHalf :: (MArray t a IO, HalfStorage b a) => Memory t a -> Address -> b -> IO ()
storeHalf = store halfToBytes 2

storeWord :: (MArray t a IO, WordStorage b a) => Memory t a -> Address -> b -> IO ()
storeWord = store wordToBytes 4

-- Write a ByteString to memory in little endian byteorder.
storeByteString :: (MArray t a IO, Conversion Word8 a) => Memory t a -> Address -> BSL.ByteString -> IO ()
storeByteString mem addr bs =
    mapM_ (\(off, val) -> storeByte mem (addr + off) (convert val))
        $ zip [0..] $ BSL.unpack bs
