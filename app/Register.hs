{-# LANGUAGE FlexibleContexts #-}

module Register where

import Numeric ( showHex )
import Data.Ix ( Ix )
import Data.Word ( Word32 )
import Data.Int ( Int32 )
import Data.Array.IO
    ( Ix, IOUArray, getElems, readArray, writeArray, MArray(newArray) )
import Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import Control.Monad (unless)

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
data RegisterFile = RegisterFile { pc   :: IORef Word32
                                 , regs :: IOUArray RegIdx Register
                                 }

-- Create a new register file.
mkRegFile :: IO RegisterFile
mkRegFile = RegisterFile <$> newIORef 0 <*> newArray (minBound, maxBound) 0

-- Dump all register values.
dumpRegs :: RegisterFile -> IO String
dumpRegs = fmap (foldr (\(a, v) s -> show a ++ "\t= 0x" ++ showHex (fromIntegral v :: Word32) "\n" ++ s) ""
        . zip [(minBound :: RegIdx)..maxBound]) . getElems . regs

------------------------------------------------------------------------

-- Read register value at given register index.
-- For the zero register, value 0 is always returned.
readRegister :: RegisterFile -> RegIdx -> IO Register
readRegister = readArray . regs

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
writeRegister RegisterFile{regs=r} idx val = unless (idx == Zero) $ writeArray r idx val

-- Read program counter value.
readPC :: RegisterFile -> IO Word32
readPC = readIORef . pc

-- | Write a new program counter value.
--
-- Examples:
--
-- >>> r <- mkRegFile
-- >>> readPC r
-- 0
-- >>> writePC r 42
-- >>> readPC r
-- 42
writePC :: RegisterFile -> Word32 -> IO ()
writePC = writeIORef . pc
