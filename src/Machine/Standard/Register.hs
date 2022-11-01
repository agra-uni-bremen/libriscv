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

-- Write register at given register index.
-- Writes to the zero register are ignored.
writeRegister :: RegisterFile -> RegIdx -> Register -> IO ()
writeRegister RegisterFile{regs=r} idx val = unless (idx == Zero) $ writeArray r idx val

-- Read program counter value.
readPC :: RegisterFile -> IO Word32
readPC = readIORef . pc

-- Write a new program counter value.
writePC :: RegisterFile -> Word32 -> IO ()
writePC = writeIORef . pc
