module Memory where

import Data.Bits
import Data.Int
import Data.Word
import Data.Array.IO

-- 32-bit addresses for RV32.
type Address = Word32

-- Byte-addressable memory.
type Memory = IOUArray Address Word8

-- Create a new memory of the given size.
mkMemory :: Word32 -> IO (Memory)
mkMemory size = do
    array <- newArray_ (0, size - 1) :: IO (Memory)
    return array

------------------------------------------------------------------------

loadByte :: Memory -> Address -> IO (Word8)
loadByte = readArray

-- TODO: Refactor using hGetArray
loadWord :: Memory -> Address -> IO (Word32)
loadWord mem addr = do
    -- TODO: Could use replicateM here to read 4 bytes as a list
    b0 <- readWord addr 0
    b1 <- readWord addr 1
    b2 <- readWord addr 2
    b3 <- readWord addr 3

    return $ b0
        .|. (b1 `shift` 8)
        .|. (b2 `shift` 16)
        .|. (b3 `shift` 24)
    where
        readWord :: Address -> Word32 -> IO (Word32)
        readWord addr off = do
            word <- loadByte mem $ addr + off
            return $ fromIntegral word

storeByte :: Memory -> Address -> Word8 -> IO ()
storeByte = writeArray

getBytes :: Word32 -> [Word8]
getBytes w = map (\off -> fromIntegral $ (shiftR w off) .&. 0xff) offs
    where
        offs = reverse $ take 4 $ iterate (+8) 0

storeWord :: Memory -> Address -> Word32 -> IO ()
storeWord mem addr word = do
    mapM (storeByte mem addr) $ getBytes word
    pure ()
