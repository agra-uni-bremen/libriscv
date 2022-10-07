{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Interpreter.Concrete.Executor where

import Types
import Data.Word
import Instructions
import Control.Monad.Free
import qualified Interpreter.Concrete.Memory as MEM
import qualified Interpreter.Concrete.Register as REG

-- Architectural state of the executor.
type ArchState = (REG.RegisterFile, MEM.Memory)

mkArchState :: Address -> Word32 -> IO ArchState
mkArchState memStart memSize = do
    reg <- REG.mkRegFile
    mem <- MEM.mkMemory memStart memSize
    pure (reg, mem)

dumpState :: ArchState -> IO ()
dumpState (r, m) =
    REG.dumpRegs r >>= putStr

instance ByteAddrsMem ArchState where
    storeByteString (_, mem) addr bs =
        MEM.storeByteString mem addr bs

------------------------------------------------------------------------

run' :: ArchState -> InstructionF Address (IO a) -> IO a
run' (regFile, _) (ReadRegister idx f) = do
    REG.readRegister regFile idx >>= f
run' (regFile, _) (WriteRegister idx reg next) = do
    REG.writeRegister regFile idx reg
    next
run' (_, mem) (LoadWord addr f) = do
    MEM.loadWord mem addr >>= f
run' (_, mem) (StoreWord addr w next) = do
    MEM.storeWord mem addr w
    next
run' (regFile, _) (WritePC w next) = do
    REG.writePC regFile w
    next
run' (regFile, _) (ReadPC f) = do
    REG.readPC regFile >>= f
run' _ UnexpectedError = fail "Unexpected error"

run :: ArchState -> Address ->  InstructionM Address a -> IO a
run s@(reg, mem) startAddr ast = do
    REG.writePC reg startAddr -- TODO: Prepend this as a node to AST

    -- XXX
    res <- iterM (run' s) ast
    pure res
