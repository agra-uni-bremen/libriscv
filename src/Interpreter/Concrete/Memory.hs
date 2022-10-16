{-# LANGUAGE TupleSections #-}
module Interpreter.Concrete.Memory where

import Types
import Utils ( fstWordLe, getBytes )
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

-- | Size of the memory
--
-- Examples:
--
-- >>> mem <- mkMemory 0x0 512
-- >>> memSize mem
-- 512
memSize :: Memory -> IO Word32
memSize = fmap ((+1) . snd) .  getBounds . snd

------------------------------------------------------------------------

loadByte :: Memory -> Address -> IO Word8
loadByte = readArray . snd

loadWord :: Memory -> Address -> IO Word32
loadWord mem addr = do
    -- TODO: Refactor, by reading bytes as a list and transforming it.
    b0 <- readWord addr 3
    b1 <- readWord addr 2
    b2 <- readWord addr 1
    b3 <- readWord addr 0

    pure $ b0 .|. (b1 `shift` 8) .|. (b2 `shift` 16) .|. (b3 `shift` 24)

    where
        readWord :: Address -> Word32 -> IO Word32
        readWord addr off = fromIntegral <$> loadByte mem (toMemAddr mem addr + off)

-- | Store a byte at the given address in memory.
--
-- Examples:
--
-- >>> mem <- mkMemory 0x0 512
-- >>> storeByte mem 0x4 0xab
-- >>> loadByte mem 0x04
-- 171
--
storeByte :: Memory -> Address -> Word8 -> IO ()
storeByte mem@(_, array) addr = writeArray array $ toMemAddr mem addr

-- | Store a word at the given address in memory.
--
-- Examples:
--
-- >>> mem <- mkMemory 0x0 256
-- >>> storeWord mem 8 0xdeadbeef
-- >>> loadWord mem 8
-- 3735928559
-- >>> loadByte mem 8
-- 222
-- >>> loadByte mem 9
-- 173
-- >>> loadByte mem 11
-- 239
--
storeWord :: Memory -> Address -> Word32 -> IO ()
storeWord mem addr =
    mapM_ (\(off, val) -> storeByte mem (addr + off) val)
        . zip [(0 :: Address)..4] . getBytes

-- | Write a ByteString to memory in little endian byteorder.
--
-- Examples:
--
-- >>> let bs = BSL.pack [0xde, 0xad, 0xbe, 0xef]
-- >>> mem <- mkMemory 0x0 32
-- >>> storeByteString mem 0x0 bs
-- >>> loadWord mem 0x0
-- 4022250974
storeByteString :: Memory -> Address -> BSL.ByteString -> IO ()
storeByteString mem addr bs =
    mapM_ (\(off, val) -> storeWord mem (addr + off) $ fstWordLe val)
        $ zip [0,4..(fromIntegral $ BSL.length bs)] lst
    where
        lst = takeWhile (not . BSL.null) $ iterate (BSL.drop 4) bs