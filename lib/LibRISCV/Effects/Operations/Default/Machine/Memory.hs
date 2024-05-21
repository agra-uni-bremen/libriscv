{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Provides an implementatio of a byte-addressable memory, intended for internal
-- usage in interpreters for the 'LibRISCV.Effects.Operations.Operations' effect.
module LibRISCV.Effects.Operations.Default.Machine.Memory
  ( Memory
  , HalfStorage(..)
  , WordStorage(..)
  , mkMemory
  , memSize
  , loadByte
  , loadHalf
  , loadWord
  , storeByte
  , storeHalf
  , storeWord
  , storeByteString
  , mkWord
  , mkBytes
  )
where

import LibRISCV
import Data.Int ()
import Data.Bits ( Bits((.|.), (.&.), shift, shiftR) )
import Data.Word ( Word8, Word16, Word32 )
import Data.Array.IO
    ( readArray, writeArray, MArray(getBounds, newArray_) )
import qualified Data.ByteString.Lazy as BSL
import Data.BitVector (BV, bitVec)

-- | Since the memory is byte-addressable it requires converting values of a
-- larger size to bytes. This type class is responsible for a conversion of
-- 16-bit values (halfs). That is, it converts halfs to bytes (and vice versa)
-- in little endian.
class HalfStorage halfType byteType where
    -- | Convert a list of two bytes to a single half.
    toHalf :: [byteType] -> halfType
    -- | Convert a single half to a list of two bytes.
    halfToBytes :: halfType -> [byteType]

-- | Similar to 'HalfStorage' but handles conversion of 32-bit values (words).
class WordStorage wordType byteType where
    -- | Convert a list of four bytes to a single word.
    toWord :: [byteType] -> wordType
    -- | Convert a single word to a list of four bytes.
    wordToBytes :: wordType -> [byteType]

instance WordStorage Word32 Word8 where
    toWord  = mkWord
    wordToBytes = mkBytes

instance HalfStorage Word16 Word8 where
    toHalf = fromIntegral . mkWord
    halfToBytes = mkBytes . fromIntegral

instance WordStorage BV Word8 where
    toWord  = (bitVec 32 :: Word32 -> BV). fromIntegral . mkWord
    wordToBytes = mkBytes . fromIntegral

instance HalfStorage BV Word8 where
    toHalf = bitVec 16 . mkWord
    halfToBytes = mkBytes . fromIntegral

-- | Converts a list of bytes to a 'Word32' in little endian.
mkWord :: [Word8] -> Word32
mkWord bytes = foldl (\x (byte,idx) -> (fromIntegral byte `shift` (idx * 8)) .|. x)
    0 $ zip bytes [0..length bytes - 1]

-- | Split a 32-bit word into four octets in little endian.
mkBytes :: Word32 -> [Word8]
mkBytes w = map (\off -> fromIntegral $ shiftR w off .&. 0xff) offs
    where
        offs = take 4 $ iterate (+8) 0

------------------------------------------------------------------------

-- | Representation of a byte-addressable memory. The type is parameterized
-- over an array implementation (such as 'Data.Array.IO.IOUArray') and a
-- generic value type (used to represent instruction operands).
data Memory t a = Memory
  { memStart :: Address
  , memBytes :: t Address a
  }

-- | Create a new memory of the given size starting at the given address.
mkMemory :: MArray t a IO => Address -> Word32 -> IO (Memory t a)
mkMemory addr size = fmap (Memory addr) (newArray_ (0, size - 1))

-- Translate global address to a memory-local address.
toMemAddr :: Memory t a -> Address -> Address
toMemAddr mem addr = addr - (memStart mem)

-- | Returns the size of the memory in bytes.
memSize :: MArray t a IO => Memory t a -> IO Word32
memSize = fmap ((+1) . snd) . getBounds . memBytes

------------------------------------------------------------------------

-- TODO: Only provide load/store, remove all wrapper functions. Could
-- use the 'Size' type from the operations language for this purpose too.

-- | Load a single byte from memory at the given address.
loadByte :: MArray t a IO => Memory t a -> Address -> IO a
loadByte mem = readArray (memBytes mem) . toMemAddr mem

load :: (MArray t a IO) => ([a] -> b) -> Word32 -> Memory t a -> Address -> IO b
load proc bytesize mem addr = proc <$>
    mapM (\off -> loadByte mem $ addr + off) [0..(bytesize - 1)]

-- | Load a half (16-bit) from memory at the given address.
loadHalf :: (MArray t a IO, HalfStorage b a) => Memory t a -> Address -> IO b
loadHalf = load toHalf 2

-- | Load a word (32-bit) from memory at the given address.
loadWord :: (MArray t a IO, WordStorage b a) => Memory t a -> Address -> IO b
loadWord = load toWord 4

-- | Store a single byte in memory.
storeByte :: MArray t a IO => Memory t a -> Address -> a -> IO ()
storeByte mem addr = writeArray (memBytes mem) $ toMemAddr mem addr

store :: (MArray t a IO) => (b -> [a]) -> Word32 -> Memory t a -> Address -> b -> IO ()
store proc bytesize mem addr =
    mapM_ (\(off, val) -> storeByte mem (addr + off) val)
        . zip [0..(bytesize - 1)] . proc

-- | Store a half (16-bit) in memory.
storeHalf :: (MArray t a IO, HalfStorage b a) => Memory t a -> Address -> b -> IO ()
storeHalf = store halfToBytes 2

-- | Store a word (32-bit) in memory.
storeWord :: (MArray t a IO, WordStorage b a) => Memory t a -> Address -> b -> IO ()
storeWord = store wordToBytes 4

-- | Write a 'BSL.ByteString' to memory in little endian byteorder. Expects a
-- function to convert single bytes ('Word8') to the chosen value
-- representation, a t'Memory', as well the 'Address' where the string should be
-- stored and the 'BSL.ByteString' itself.
storeByteString :: (MArray t a IO) =>
  (Word8 -> a) ->
  Memory t a ->
  Address ->
  BSL.ByteString ->
  IO ()
storeByteString convert mem addr bs =
    mapM_ (\(off, val) -> storeByte mem (addr + off) (convert val))
        $ zip [0..] $ BSL.unpack bs
