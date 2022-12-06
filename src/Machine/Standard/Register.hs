{-# LANGUAGE FlexibleContexts #-}
module Machine.Standard.Register where

import Common.Types
import Data.Word ( Word32 )
import Data.Array.IO
    ( getElems, readArray, writeArray, MArray(newArray) )
import Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import Control.Monad (unless)

-- Register file addressed by RegIdx containing Word32.
data RegisterFile t a = RegisterFile { pc   :: IORef Word32
                                     , regs :: t RegIdx a
                                     }

-- Create a new register file.
mkRegFile :: MArray t a IO => a -> IO (RegisterFile t a)
mkRegFile defValue = RegisterFile <$> newIORef 0 <*> newArray (minBound, maxBound) defValue

-- Dump all register values.
dumpRegs :: MArray t a IO => (a -> ShowS) -> RegisterFile t a -> IO String
dumpRegs sh = fmap (foldr (\(a, v) s -> show a ++ "\t= 0x" ++ ((sh v) "\n") ++ s) ""
        . zip [(minBound :: RegIdx)..maxBound]) . getElems . regs

------------------------------------------------------------------------

-- Read register value at given register index.
-- For the zero register, value 0 is always returned.
readRegister :: MArray t a IO => RegisterFile t a -> RegIdx -> IO a
readRegister = readArray . regs

-- Write register at given register index.
-- Writes to the zero register are ignored.
writeRegister :: MArray t a IO => RegisterFile t a -> RegIdx -> a -> IO ()
writeRegister RegisterFile{regs=r} idx val = unless (idx == Zero) $ writeArray r idx val

-- Read program counter value.
readPC :: RegisterFile t a -> IO Word32
readPC = readIORef . pc

-- Write a new program counter value.
writePC :: RegisterFile t a -> Word32 -> IO ()
writePC = writeIORef . pc
