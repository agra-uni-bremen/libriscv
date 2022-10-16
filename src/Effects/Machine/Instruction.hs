{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
module Effects.Machine.Instruction where

import Data.Bits
import Common.Types (Address)
import Common.Types
import Decoder
import Data.Word
import Control.Monad (when)
import Control.Monad.Freer 
import Control.Monad.Freer.TH

import Effects.Logging.InstructionFetch
import qualified Common.Machine.Standard.Register as REG
import qualified Common.Machine.Standard.Memory as MEM

data Instruction r where
    ReadRegister :: RegIdx -> Instruction Register
    WriteRegister :: RegIdx -> Register -> Instruction ()
    LoadWord :: Address -> Instruction Word32
    StoreWord :: Address ->  Word32 -> Instruction ()
    WritePC :: Word32 -> Instruction ()
    ReadPC :: Instruction Word32
    UnexpectedError :: Instruction r

makeEffect ''Instruction

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

runInstructionM :: forall r effs . LastMember IO effs => ArchState -> Eff (Instruction ': effs) r -> Eff effs r 
runInstructionM (regFile, mem) = interpretM $ \case
    (ReadRegister idx) -> REG.readRegister regFile idx
    (WriteRegister idx reg) -> REG.writeRegister regFile idx reg
    (LoadWord addr) -> MEM.loadWord mem addr
    (StoreWord addr w) -> MEM.storeWord mem addr w
    (WritePC w) -> REG.writePC regFile w
    ReadPC -> REG.readPC regFile
    UnexpectedError -> fail "Unexpected error"