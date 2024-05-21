{-# LANGUAGE FlexibleContexts #-}

-- | Provides a polymorphic implementation of a register file. This module is
-- intended to be used internally by interpreters for the
-- 'LibRISCV.Effects.Operations.Operations' effect. This register file
-- implementation also provides facilities for storing a concrete program
-- counter.
module LibRISCV.Effects.Operations.Default.Machine.Register where

import Control.Monad (unless)
import Data.Array.IO (
    MArray (newArray),
    getElems,
    readArray,
    writeArray,
 )
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Word (Word32)
import LibRISCV

-- | Register file addressed by 'RegIdx'. The type is parameterized over an array
-- implementation (such as 'Data.Array.IO.IOUArray') and a generic value type
-- (used to represent instruction operands).
data RegisterFile t a = RegisterFile
    { pc :: IORef Word32
    -- ^ The current program counter (always concrete in this implementation).
    , regs :: t RegIdx a
    -- ^ The underlying array to store the register values.
    }

-- | Create a new register file, initializing all registers with the given default
-- value. This value /must/ represent the zero value in the chosen value type.
mkRegFile :: (MArray t a IO) => a -> IO (RegisterFile t a)
mkRegFile defValue = RegisterFile <$> newIORef 0 <*> newArray (minBound, maxBound) defValue

-- | Dump the current register file state as a 'String'.
dumpRegs :: (MArray t a IO) => (a -> ShowS) -> RegisterFile t a -> IO String
dumpRegs sh =
    fmap
        ( foldr (\(a, v) s -> show a ++ "\t= 0x" ++ sh v "\n" ++ s) ""
            . zip [(minBound :: RegIdx) .. maxBound]
        )
        . getElems
        . regs

------------------------------------------------------------------------

-- | Read register value at given register index. For the 'Zero' register index, the
-- zero/default value (as passed to 'mkRegFile' is always returned.
readRegister :: (MArray t a IO) => RegisterFile t a -> RegIdx -> IO a
readRegister = readArray . regs

-- | Write register at given register index. Writes to the 'Zero' register are ignored.
writeRegister :: (MArray t a IO) => RegisterFile t a -> RegIdx -> a -> IO ()
writeRegister RegisterFile{regs = r} idx val = unless (idx == Zero) $ writeArray r idx val

------------------------------------------------------------------------

-- | Returs the current program counter value.
readPC :: RegisterFile t a -> IO Word32
readPC = readIORef . pc

-- | Write a new program counter value.
writePC :: RegisterFile t a -> Word32 -> IO ()
writePC = writeIORef . pc
