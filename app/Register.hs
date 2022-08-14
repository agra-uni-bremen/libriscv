{-# LANGUAGE FlexibleContexts #-}

module Register where

import Data.Ix
import Data.Word
import Data.Int
import Data.Array.IO

-- | Type to index for the register file.
--
-- >>> toEnum 12 :: RegIdx
-- A2
-- >>> toEnum 31 :: RegIdx
-- T6
-- >>> fromEnum (minBound :: RegIdx)
-- 0
--
data RegIdx = Zero | RA | SP | GP | TP | T0 | T1 | T2 | FP
    | S1 | A0 | A1 | A2 | A3 | A4 | A5 | A6 | A7 | S2 | S3
    | S4 | S5 | S6 | S7 | S8 | S9 | S10 | S11 | T3 | T4
    | T5 | T6 deriving (Ord, Eq, Ix, Bounded, Show, Enum)

-- Type used to represent RISC-V registers.
type Register = Int32

-- Register file addressed by RegIdx containing Word32.
type RegisterFile = IOUArray RegIdx Register

-- Create a new register file.
mkRegFile :: IO (RegisterFile)
mkRegFile = newArray (minBound, maxBound) 0

-- Dump all register values.
dumpRegs :: RegisterFile -> IO (String)
dumpRegs r = do
    e <- getElems r
    return $ foldr (\(a, v) s -> (show a) ++ "\t= " ++ (show v) ++ "\n" ++ s) ""
        $ zip [(minBound :: RegIdx)..maxBound] e

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
-- >>> writeRegister r A1 23
-- >>> readRegister r A1
-- 23
--
-- >>> r <- mkRegFile
-- >>> writeRegister r Zero 42
-- >>> readRegister r Zero
-- 0
--
writeRegister :: RegisterFile -> RegIdx -> Register -> IO ()
writeRegister r idx val
    | idx == Zero = pure () -- ignore writes to zero register
    | otherwise = writeArray r idx val
