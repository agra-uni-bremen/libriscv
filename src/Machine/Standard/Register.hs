{-# LANGUAGE FlexibleContexts #-}
module Machine.Standard.Register where

import Common.Types
import Data.Ix ( Ix )
import Data.Word ( Word32 )
import Data.Int ( Int32 )
import Data.Array.IO
    ( Ix, IOArray, getElems, readArray, writeArray, MArray(newArray) )
import Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import Control.Monad (unless)

-- Register file addressed by RegIdx containing Word32.
data RegisterFile a = RegisterFile { pc   :: IORef Word32
                                   , regs :: IOArray RegIdx a
                                   }

-- Create a new register file.
mkRegFile :: a -> IO (RegisterFile a)
mkRegFile defValue = RegisterFile <$> newIORef 0 <*> newArray (minBound, maxBound) defValue

-- Dump all register values.
dumpRegs :: (a -> ShowS) -> RegisterFile a -> IO String
dumpRegs sh = fmap (foldr (\(a, v) s -> show a ++ "\t= 0x" ++ ((sh v) "\n") ++ s) ""
        . zip [(minBound :: RegIdx)..maxBound]) . getElems . regs

------------------------------------------------------------------------

-- Read register value at given register index.
-- For the zero register, value 0 is always returned.
readRegister :: RegisterFile a -> RegIdx -> IO a
readRegister = readArray . regs

-- Write register at given register index.
-- Writes to the zero register are ignored.
writeRegister :: RegisterFile a -> RegIdx -> a -> IO ()
writeRegister RegisterFile{regs=r} idx val = unless (idx == Zero) $ writeArray r idx val

-- Read program counter value.
readPC :: RegisterFile a -> IO Word32
readPC = readIORef . pc

-- Write a new program counter value.
writePC :: RegisterFile a -> Word32 -> IO ()
writePC = writeIORef . pc
