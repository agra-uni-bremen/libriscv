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

buildInstruction' :: forall v r. (Num v, Ord v, Bits v, Conversion v (Expr v), Conversion v Address, Member (Instruction v) r, Member LogInstructionFetch r) => v -> InstructionType -> Eff r ()
buildInstruction' _ ADD{..} = do
    r1 <- readRegister @v rs1
    r2 <- readRegister @v rs2
    writeRegister @v rd $ (convert r1) :+: r2
    buildInstruction @v
buildInstruction' _ ADDI{..} = do
    r1 <- readRegister @v rs1
    writeRegister @v rd $ LossyConvert ((SExt r1) :+: imm)
    buildInstruction @v
buildInstruction' _ LW{..} = do
    r1 <- readRegister @v rs1
    -- TODO: Alignment handling
    word <- loadWord @v $ LossyConvert ((SExt r1) :+: imm)
    writeRegister @v rd $ convert word
    buildInstruction @v
buildInstruction' _ SW{..} = do
    r1 <- readRegister @v rs1
    r2 <- readRegister @v rs2
    storeWord @v (LossyConvert $ (SExt r1) :+: imm) $ (convert @v r2)
    buildInstruction @v
-- buildInstruction' pc BLT{..} = do
--     r1 <- readRegister @v rs1
--     r2 <- readRegister @v rs2
--     -- TODO: Alignment handling
--     let cond = (convert r1 :: Expr v) :<: (convert r2 :: Expr v)
--     whenM (liftE cond) $
--         writePC @v $ LossyConvert ((SExt pc) :+: imm)
--     buildInstruction @v
buildInstruction' pc JAL{..} = do
    nextInstr <- readPC
    -- TODO: Alignment handling
    writePC @v $ LossyConvert ((SExt pc) :+: imm)
    writeRegister @v rd $ (convert @v nextInstr)
    buildInstruction @v
buildInstruction' pc JALR{..} = do
    nextInstr <- readPC
    r1 <- readRegister @v rs1
    writePC @v $ (LossyConvert ((SExt r1) :+: imm)) :&: (0xfffffffe :: v)
    writeRegister @v rd $ (convert @v nextInstr)
    buildInstruction @v
buildInstruction' _ LUI{..} = do
    writeRegister @v rd $ LossyConvert (Signed imm)
    buildInstruction @v
buildInstruction' pc AUIPC{..} = do
    writeRegister @v rd $ LossyConvert ((SExt pc) :+: imm)
    buildInstruction @v
buildInstruction' _ InvalidInstruction = pure () -- XXX: ignore for now

------------------------------------------------------------------------

buildInstruction :: forall v r . (Num v, Ord v, Bits v, Conversion v (Expr v), Conversion v Address, Member (Instruction v) r, Member LogInstructionFetch r) => Eff r ()
buildInstruction = do
    -- fetch & decode instruction at current PC
    pc <- readPC @v
    let pcExpr = (convert pc :: Expr v)
    instrWord <- loadWord pcExpr
    let inst = decode $ convert instrWord

    logFetched (convert pc) inst

    -- Increment PC before execute', allows setting PC to to
    -- different values in execute' for jumps and branches.
    writePC $ pcExpr :+: (4 :: v)

    buildInstruction' pc inst

buildAST :: forall v r . (Num v, Ord v, Bits v, Conversion v (Expr v), Conversion v Address, Member (Instruction v) r, Member LogInstructionFetch r) => Expr v -> Expr v -> Eff r ()
buildAST entry sp = writePC @v entry >> writeRegister @v SP sp >> buildInstruction @v
