module Register where

import Data.Word
import Data.Array.IO

-- Use Word8 for register index as maximum value is 32.
type RegisterIndex = Word8

-- Type used to represent RISC-V registers.
type Register = Word32

-- Register file addressed by Word8 containing Word32.
type RegisterFile = IOUArray RegisterIndex Register

-- Create a new register file.
mkRegFile :: IO (RegisterFile)
mkRegFile = newArray (0, 31) 0

-- Read register value at given register index.
-- For the zero register, value 0 is always returned.
readRegister :: RegisterFile -> RegisterIndex -> IO (Register)
readRegister = readArray

-- | Write register at given register index.
--   Writes to the zero register are ignored.
--
-- Examples:
--
-- >>> r <- mkRegFile
-- >>> writeRegister r 1 23
-- >>> readRegister r 1
-- 23
--
-- >>> r <- mkRegFile
-- >>> writeRegister r 0 42
-- >>> readRegister r 0
-- 0
--
writeRegister :: RegisterFile -> RegisterIndex -> Register -> IO ()
writeRegister r idx val
    | idx == 0 = pure () -- ignore writes to zero register
    | otherwise = writeArray r idx val
