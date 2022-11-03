{-# LANGUAGE FlexibleContexts #-}
module Machine.Standard.Register where

import Common.Types
import Numeric ( showHex )
import Data.Ix ( Ix )
import Data.Word ( Word32 )
import Data.Int ( Int32 )
import Data.Array.IO
    ( Ix, IOUArray, getElems, readArray, writeArray, MArray(newArray) )
import Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import Control.Monad (unless)

-- Register file addressed by RegIdx containing Word32.
data RegisterFile a = RegisterFile { pc   :: IORef Word32
                                   , regs :: IOUArray RegIdx a
                                   }

-- Create a new register file.
mkRegFile :: MArray IOUArray a IO => a -> IO (RegisterFile a)
mkRegFile defValue = RegisterFile <$> newIORef 0 <*> newArray (minBound, maxBound) defValue

-- Dump all register values.
dumpRegs :: (Integral a, MArray IOUArray a IO) => RegisterFile a -> IO String
dumpRegs = fmap (foldr (\(a, v) s -> show a ++ "\t= 0x" ++ showHex (fromIntegral v :: Word32) "\n" ++ s) ""
        . zip [(minBound :: RegIdx)..maxBound]) . getElems . regs

------------------------------------------------------------------------

-- Read register value at given register index.
-- For the zero register, value 0 is always returned.
readRegister :: MArray IOUArray a IO => RegisterFile a -> RegIdx -> IO a
readRegister = readArray . regs

-- Write register at given register index.
-- Writes to the zero register are ignored.
writeRegister :: MArray IOUArray a IO => RegisterFile a -> RegIdx -> a -> IO ()
writeRegister RegisterFile{regs=r} idx val = unless (idx == Zero) $ writeArray r idx val

-- Read program counter value.
readPC :: RegisterFile a -> IO Word32
readPC = readIORef . pc

-- Write a new program counter value.
writePC :: RegisterFile a -> Word32 -> IO ()
writePC = writeIORef . pc
