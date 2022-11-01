{-# LANGUAGE FlexibleContexts #-}
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

------------------------------------------------------------------------

buildInstruction' :: (Member Instruction r, Member LogInstructionFetch r) => Expr Word32 -> InstructionType -> Eff r ()
buildInstruction' _ ADD{..} = do
    r1 <- readRegister rs1
    r2 <- readRegister rs2
    writeRegister rd $ r1 :+: r2
    buildInstruction
buildInstruction' _ ADDI{..} = do
    r1 <- readRegister rs1
    writeRegister rd $ r1 :+: imm
    buildInstruction
buildInstruction' _ LW{..} = do
    r1 <- readRegister rs1
    -- TODO: Alignment handling
    word <- loadWord $ LossyConvert (r1 :+: imm)
    writeRegister rd $ LossyConvert @Unsigned32 @Signed32 word
    buildInstruction
buildInstruction' _ SW{..} = do
    r1 <- readRegister rs1
    r2 <- readRegister rs2
    storeWord (LossyConvert $ r1 :+: imm) $ LossyConvert r2
    buildInstruction
buildInstruction' pc BLT{..} = do
    r1 <- readRegister rs1
    r2 <- readRegister rs2
    -- TODO: Alignment handling
    whenM (liftE $ r1 :<: r2) $
        writePC $ LossyConvert $ LossyConvert @Unsigned32 @Signed32 pc :+: imm
    buildInstruction
buildInstruction' pc JAL{..} = do
    nextInstr <- readPC
    -- TODO: Alignment handling
    writePC $ LossyConvert $ LossyConvert @Unsigned32 @Signed32 pc :+: imm
    writeRegister rd $ LossyConvert @Unsigned32 @Signed32 nextInstr
    buildInstruction
buildInstruction' pc JALR{..} = do
    nextInstr <- readPC
    rs1Val <- readRegister rs1
    writePC $ LossyConvert (rs1Val :+: imm) :&: (0xfffffffe :: Unsigned32)
    writeRegister rd $ LossyConvert @Unsigned32 @Signed32 nextInstr
    buildInstruction
buildInstruction' _ LUI{..} = do
    writeRegister rd imm
    buildInstruction
buildInstruction' pc AUIPC{..} = do
    writeRegister rd $ LossyConvert @Unsigned32 @Signed32 pc :+: imm
    buildInstruction
buildInstruction' _ InvalidInstruction = pure () -- XXX: ignore for now

------------------------------------------------------------------------

buildInstruction :: (Member Instruction r, Member LogInstructionFetch r) => Eff r ()
buildInstruction = do
    -- fetch & decode instruction at current PC
    pc <- readPC
    instrWord <- loadWord pc
    let inst = decode $ convert instrWord

    logFetched (convert pc) inst

    -- Increment PC before execute', allows setting PC to to
    -- different values in execute' for jumps and branches.
    writePC $ pc :+: (4 :: Unsigned32)

    buildInstruction' pc inst

buildAST :: (Member Instruction r, Member LogInstructionFetch r) => Word32 -> Register -> Eff r ()
buildAST entry sp = writePC (convert entry) >> writeRegister SP sp >> buildInstruction
