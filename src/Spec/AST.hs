{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
module Spec.AST where

import Common.Types
import Decoder
import Data.Word
import Control.Monad.Freer

import Spec.Expr
import Common.Utils (whenMword,unlessMword)
import Effects.Logging.InstructionFetch
import Conversion
import Spec.Instruction

------------------------------------------------------------------------

-- We require type annotations here to workaround a limitation of
-- GHC type inference in conjunction with freer-simple. Alternatively,
-- we could use a proxy type.
--
-- See: https://github.com/lexi-lambda/freer-simple/issues/7

buildInstruction'' :: forall v r. (Conversion v Word32, Member (Instruction v) r) => v -> InstructionType -> Eff r ()
buildInstruction'' _ ADDI{..} = do
    r1 <- readRegister rs1
    writeRegister @v rd $ r1 `addSInt` imm
buildInstruction'' _ SLTI{..} = do
    r1 <- readRegister rs1
    let cond = (FromImm r1) `Slt` (FromInt imm) :: Expr v
    writeRegister @v rd $ convert cond
buildInstruction'' _ SLTIU{..} = do
    r1 <- readRegister rs1
    let cond = (FromImm r1) `Ult` (FromInt imm) :: Expr v
    writeRegister @v rd $ convert cond
buildInstruction'' _ ANDI{..} = do
    r1 <- readRegister rs1
    writeRegister @v rd $ r1 `andInt` imm
buildInstruction'' _ ORI{..} = do
    r1 <- readRegister rs1
    writeRegister @v rd $ r1 `orInt` imm
buildInstruction'' _ XORI{..} = do
    r1 <- readRegister rs1
    writeRegister @v rd $ r1 `xorInt` imm
buildInstruction'' _ SLLI{..} = do
    r1 <- readRegister rs1
    writeRegister @v rd $ r1 `lshlInt` shamt
buildInstruction'' _ SRLI{..} = do
    r1 <- readRegister rs1
    writeRegister @v rd $ r1 `lshrInt` shamt
buildInstruction'' _ SRAI{..} = do
    r1 <- readRegister rs1
    writeRegister @v rd $ r1 `ashrInt` shamt
buildInstruction'' _ LUI{..} = do
    writeRegister @v rd $ FromInt imm
buildInstruction'' pc AUIPC{..} = do
    writeRegister @v rd $ pc `addSInt` imm
buildInstruction'' _ ADD{..} = do
    r1 <- readRegister rs1
    r2 <- readRegister rs2
    writeRegister @v rd $ r1 `addSImm` r2
buildInstruction'' _ SLT{..} = do
    r1 <- readRegister rs1
    r2 <- readRegister rs2
    let cond = (FromImm r1) `Slt` (FromImm r2) :: Expr v
    writeRegister @v rd $ convert cond
buildInstruction'' _ SLTU{..} = do
    r1 <- readRegister rs1
    r2 <- readRegister rs2
    let cond = (FromImm r1) `Ult` (FromImm r2) :: Expr v
    writeRegister @v rd $ convert cond
buildInstruction'' _ AND{..} = do
    r1 <- readRegister rs1
    r2 <- readRegister rs2
    writeRegister @v rd $ r1 `andImm` r2
buildInstruction'' _ OR{..} = do
    r1 <- readRegister rs1
    r2 <- readRegister rs2
    writeRegister @v rd $ r1 `orImm` r2
buildInstruction'' _ XOR{..} = do
    r1 <- readRegister rs1
    r2 <- readRegister rs2
    writeRegister @v rd $ r1 `xorImm` r2
buildInstruction'' _ SLL{..} = do
    r1 <- readRegister rs1
    r2 <- readRegister rs2
    writeRegister @v rd $ (FromImm r1) `LShl` (regShamt $ FromImm r2)
buildInstruction'' _ SRL{..} = do
    r1 <- readRegister rs1
    r2 <- readRegister rs2
    writeRegister @v rd $ (FromImm r1) `LShr` (regShamt $ FromImm r2)
buildInstruction'' _ SUB{..} = do
    r1 <- readRegister rs1
    r2 <- readRegister rs2
    writeRegister @v rd $ (FromImm r1) `Sub` (FromImm r2)
buildInstruction'' _ SRA{..} = do
    r1 <- readRegister rs1
    r2 <- readRegister rs2
    writeRegister @v rd $ (FromImm r1) `AShr` (regShamt $ FromImm r2)
buildInstruction'' pc JAL{..} = do
    nextInstr <- readPC
    -- TODO: Alignment 
    writePC @v $ pc `addSInt` imm
    writeRegister @v rd (FromImm nextInstr)
buildInstruction'' _ JALR{..} = do
    nextInstr <- readPC
    r1 <- readRegister rs1
    writePC @v $ (r1 `addSInt` imm) `And` (FromUInt 0xfffffffe)
    writeRegister @v rd $ FromImm nextInstr
buildInstruction'' _ LB{..} = do
    r1 <- readRegister rs1
    -- TODO: Alignment handling
    byte <- loadByte @v $ r1 `addSInt` imm
    writeRegister @v rd (SExtByte $ FromImm byte)
buildInstruction'' _ LBU{..} = do
    r1 <- readRegister rs1
    -- TODO: Alignment handling
    byte <- loadByte @v $ r1 `addSInt` imm
    writeRegister @v rd (ZExtByte $ FromImm byte)
buildInstruction'' _ LH{..} = do
    r1 <- readRegister rs1
    -- TODO: Alignment handling
    half <- loadHalf $ r1 `addSInt` imm
    writeRegister @v rd (SExtHalf $ FromImm half)
buildInstruction'' _ LHU{..} = do
    r1 <- readRegister rs1
    -- TODO: Alignment handling
    half <- loadHalf $ r1 `addSInt` imm
    writeRegister @v rd (ZExtHalf $ FromImm half)
buildInstruction'' _ LW{..} = do
    r1 <- readRegister rs1
    -- TODO: Alignment handling
    word <- loadWord $ r1 `addSInt` imm
    writeRegister @v rd (FromImm word)
buildInstruction'' _ SB{..} = do
    r1 <- readRegister rs1
    r2 <- readRegister rs2
    storeByte @v (r1 `addSInt` imm) $ FromImm r2
buildInstruction'' _ SH{..} = do
    r1 <- readRegister rs1
    r2 <- readRegister rs2
    storeHalf @v (r1 `addSInt` imm) $ FromImm r2
buildInstruction'' _ SW{..} = do
    r1 <- readRegister rs1
    r2 <- readRegister rs2
    storeWord @v (r1 `addSInt` imm) $ FromImm r2
buildInstruction'' pc BEQ{..} = do
    r1 <- readRegister rs1
    r2 <- readRegister rs2
    -- TODO: Alignment handling
    let cond = (FromImm r1) `Eq` (FromImm r2)
    whenMword (convert @v <$> liftE cond) $
        writePC @v $ (FromImm pc) `AddS` (FromInt imm)
buildInstruction'' pc BNE{..} = do
    r1 <- readRegister rs1
    r2 <- readRegister rs2
    -- TODO: Alignment handling
    let cond = (FromImm r1) `Eq` (FromImm r2)
    unlessMword (convert @v <$> liftE cond) $
        writePC @v $ (FromImm pc) `AddS` (FromInt imm)
buildInstruction'' pc BLT{..} = do
    r1 <- readRegister rs1
    r2 <- readRegister rs2
    -- TODO: Alignment handling
    let cond = (FromImm r1) `Slt` (FromImm r2)
    whenMword (convert @v <$> liftE cond) $
        writePC @v $ (FromImm pc) `AddS` (FromInt imm)
buildInstruction'' pc BLTU{..} = do
    r1 <- readRegister rs1
    r2 <- readRegister rs2
    -- TODO: Alignment handling
    let cond = (FromImm r1) `Ult` (FromImm r2)
    whenMword (convert @v <$> liftE cond) $
        writePC @v $ (FromImm pc) `AddS` (FromInt imm)
buildInstruction'' pc BGE{..} = do
    r1 <- readRegister rs1
    r2 <- readRegister rs2
    -- TODO: Alignment handling
    let cond = (FromImm r1) `Sge` (FromImm r2)
    whenMword (convert @v <$> liftE cond) $
        writePC @v $ (FromImm pc) `AddS` (FromInt imm)
buildInstruction'' pc BGEU{..} = do
    r1 <- readRegister rs1
    r2 <- readRegister rs2
    -- TODO: Alignment handling
    let cond = (FromImm r1) `Uge` (FromImm r2)
    whenMword (convert @v <$> liftE cond) $
        writePC @v $ (FromImm pc) `AddS` (FromInt imm)
buildInstruction'' _ FENCE = pure () -- XXX: ignore for now
buildInstruction'' pc ECALL = ecall @v pc
buildInstruction'' pc EBREAK = ebreak @v pc

buildInstruction' :: forall v r. (Conversion v Word32, Member (Instruction v) r, Member LogInstructionFetch r) => v -> InstructionType -> Eff r ()
buildInstruction' _ InvalidInstruction = pure () -- XXX: ignore for now
buildInstruction' pc inst = do
    buildInstruction'' @v pc inst
    buildInstruction @v

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
