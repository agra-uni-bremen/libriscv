{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
module Interpreter.Concrete.Executor where

import Types
import Data.Word
import Instructions
import qualified Interpreter.Concrete.Memory as MEM
import qualified Interpreter.Concrete.Register as REG
import Interpreter.Logging.InstructionFetch (LogInstructionFetch)
import Control.Monad.Freer

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

runInstructionM :: forall r effs . LastMember IO effs => ArchState -> Eff (InstructionF ': effs) r -> Eff effs r 
runInstructionM (regFile, mem) = interpretM $ \case
    (ReadRegister idx) -> REG.readRegister regFile idx
    (WriteRegister idx reg) -> REG.writeRegister regFile idx reg
    (LoadWord addr) -> MEM.loadWord mem addr
    (StoreWord addr w) -> MEM.storeWord mem addr w
    (WritePC w) -> REG.writePC regFile w
    ReadPC -> REG.readPC regFile
    UnexpectedError -> fail "Unexpected error"