{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Spec.AST where

import Data.Bits
import Common.Types
import Decoder
import Data.Word
import Control.Monad.Freer
import Control.Monad.Freer.TH

import Spec.Expr
import Data.Int
import Common.Utils (whenM)
import Effects.Logging.InstructionFetch
import Conversion

data Instruction v r where
    ReadRegister :: RegIdx -> Instruction v v
    WriteRegister :: RegIdx -> Expr v -> Instruction v ()
    LoadWord :: Expr v -> Instruction v v
    StoreWord :: Expr v -> Expr v -> Instruction v ()
    WritePC :: Expr v -> Instruction v ()
    ReadPC :: Instruction v v
    LiftE :: Expr v -> Instruction v v

makeEffect ''Instruction

------------------------------------------------------------------------

buildInstruction' :: forall v r. (Conversion v Word32, Member (Instruction v) r, Member LogInstructionFetch r) => v -> InstructionType -> Eff r ()
buildInstruction' _ ADD{..} = do
    r1 <- readRegister @v rs1
    r2 <- readRegister @v rs2
    writeRegister @v rd $ r1 `addSImm` r2
    buildInstruction @v
buildInstruction' _ ADDI{..} = do
    r1 <- readRegister @v rs1
    writeRegister @v rd $ r1 `addSInt` imm
    buildInstruction @v
buildInstruction' _ LW{..} = do
    r1 <- readRegister @v rs1
    -- TODO: Alignment handling
    word <- loadWord @v $ r1 `addSInt` imm
    writeRegister @v rd (FromImm word)
    buildInstruction @v
buildInstruction' _ SW{..} = do
    r1 <- readRegister @v rs1
    r2 <- readRegister @v rs2
    storeWord @v (r1 `addSInt` imm) $ FromImm r2
    buildInstruction @v
buildInstruction' pc BLT{..} = do
    r1 <- readRegister @v rs1
    r2 <- readRegister @v rs2
    -- TODO: Alignment handling
    let cond = (FromImm r1) `Slt` (FromImm r2)
    whenM (liftE cond >>= pure . convert) $
        writePC @v $ (FromImm pc) `AddS` (FromInt imm)
    buildInstruction @v
buildInstruction' pc JAL{..} = do
    nextInstr <- readPC
    -- TODO: Alignment handling
    writePC @v $ pc `addSInt` imm
    writeRegister @v rd (FromImm nextInstr)
    buildInstruction @v
buildInstruction' pc JALR{..} = do
    nextInstr <- readPC
    r1 <- readRegister @v rs1
    writePC @v $ (r1 `addSInt` imm) `BAnd` (FromUInt 0xfffffffe)
    writeRegister @v rd $ FromImm nextInstr
    buildInstruction @v
buildInstruction' _ LUI{..} = do
    writeRegister @v rd $ FromInt imm
    buildInstruction @v
buildInstruction' pc AUIPC{..} = do
    writeRegister @v rd $ pc `addSInt` imm
    buildInstruction @v
buildInstruction' _ InvalidInstruction = pure () -- XXX: ignore for now

------------------------------------------------------------------------

buildInstruction :: forall v r . (Conversion v Word32, Member (Instruction v) r, Member LogInstructionFetch r) => Eff r ()
buildInstruction = do
    -- fetch & decode instruction at current PC
    pc <- readPC @v
    instrWord <- loadWord $ FromImm pc
    let inst = decode $ convert instrWord

    logFetched (convert pc) inst

    -- Increment PC before execute', allows setting PC to to
    -- different values in execute' for jumps and branches.
    writePC $ (FromImm pc) `AddU` (FromInt 4)

    buildInstruction' pc inst

buildAST :: forall v r . (Conversion v Word32, Member (Instruction v) r, Member LogInstructionFetch r) => v -> v -> Eff r ()
buildAST entry sp = writePC @v (FromImm entry) >> writeRegister @v SP (FromImm sp) >> buildInstruction @v
