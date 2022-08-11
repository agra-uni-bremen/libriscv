{-# LANGUAGE FlexibleContexts #-}

module Register where

import Data.Word
import Data.Array.IO

-- Type to index the register file
type RegIdx = Word8

-- Type used to represent RISC-V registers.
type Register = Word32
-- TODO: Custom show instance

-- Register file addressed by Word8 containing Word32.
type RegisterFile = IOUArray RegIdx Register

-- Create a new register file.
mkRegFile :: IO (RegisterFile)
mkRegFile = newArray (0, 31) 0

-- Dump all register values.
dumpRegs :: RegisterFile -> IO (String)
dumpRegs r = do
    e <- getElems r
    return $ foldr (\(a, v) s -> "regs[" ++ (show a) ++ "] = " ++ (show v) ++ "\n" ++ s) ""
        $ zip [0..length e] e

------------------------------------------------------------------------

-- Read register value at given register index.
-- For the zero register, value 0 is always returned.
readRegister :: RegisterFile -> RegIdx -> IO (Register)
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
writeRegister :: RegisterFile -> RegIdx -> Register -> IO ()
writeRegister r idx val
    | idx == 0 = pure () -- ignore writes to zero register
    | otherwise = writeArray r idx val
