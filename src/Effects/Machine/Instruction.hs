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
{-# LANGUAGE RankNTypes #-}
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
import Effects.Machine.Expression
import Conversion

data Instruction r where
    ReadRegister :: RegIdx -> Instruction (Expr Register)
    WriteRegister :: Conversion a (Expr Register) => RegIdx -> a -> Instruction ()
    LoadWord :: Expr Address -> Instruction (Expr Unsigned32)
    StoreWord :: Expr Address -> Expr Unsigned32 -> Instruction ()
    WritePC :: Expr Address -> Instruction ()
    ReadPC :: Instruction (Expr Address)
    LiftE :: Expr a -> Instruction a 
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

runInstructionM :: forall r effs . LastMember IO effs => (forall a . Expr a -> a) -> ArchState -> Eff (Instruction ': effs) r -> Eff effs r 
runInstructionM evalE (regFile, mem) = interpretM $ \case
    (ReadRegister idx) -> Signed <$> REG.readRegister regFile idx
    (WriteRegister idx reg) -> REG.writeRegister regFile idx (evalE $ convert reg)
    (LoadWord addr) -> Unsigned <$> MEM.loadWord mem (evalE addr)
    (StoreWord addr w) -> MEM.storeWord mem (evalE addr) (evalE w)
    (WritePC w) -> REG.writePC regFile (evalE w)
    ReadPC -> Unsigned <$> REG.readPC regFile
    LiftE e -> pure $ evalE e
    UnexpectedError -> fail "Unexpected error"