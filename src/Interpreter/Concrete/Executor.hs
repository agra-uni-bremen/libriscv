{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Interpreter.Concrete.Executor where

import Types
import Data.Word
import Instructions
import Control.Monad.Free
import qualified Interpreter.Concrete.Memory as MEM
import qualified Interpreter.Concrete.Register as REG
import qualified Interpreter.Logging.InstructionFetch as Logging (evalIO) 
import Common.Coproduct
import Interpreter.Logging.InstructionFetch (LogInstructionFetch)
import Common.Effects.Noop

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
    storeByteString (_, mem) = MEM.storeByteString mem

------------------------------------------------------------------------

-- Interpreter 
evalIO :: ArchState -> InstructionF Address (IO a) -> IO a
evalIO (regFile, _) (ReadRegister idx f) = REG.readRegister regFile idx >>= f
evalIO (regFile, _) (WriteRegister idx reg next) = REG.writeRegister regFile idx reg >> next
evalIO (_, mem) (LoadWord addr f) = MEM.loadWord mem addr >>= f
evalIO (_, mem) (StoreWord addr w next) = MEM.storeWord mem addr w >> next
evalIO (regFile, _) (WritePC w next) = REG.writePC regFile w >> next
evalIO (regFile, _) (ReadPC f) = REG.readPC regFile >>= f
evalIO _ UnexpectedError = fail "Unexpected error"



-- Runner / Executer whatever 
runTraceAndIO :: ArchState -> Free (InstructionF Address :+: LogInstructionFetch) a -> IO a
runTraceAndIO st = iterM (evalIO' st) where
    evalIO' st (InL f) = evalIO st f
    evalIO' st (InR g) = Logging.evalIO g
    

runIO :: (Functor g, Noop g) => ArchState -> Free (InstructionF Address :+: g) a -> IO a
runIO st = iterM (evalIO' st) where
    evalIO' st (InL f) = evalIO st f
    evalIO' st (InR g) = noop g