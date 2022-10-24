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
module Descriptions.Standard.AST (buildAST) where

import Data.Bits
import Common.Types (Address)
import Common.Types
import Decoder
import Data.Word
import Control.Monad (when)
import Control.Monad.Freer 
import Control.Monad.Freer.TH

import Effects.Logging.InstructionFetch
import Effects.Machine.Instruction 

buildInstruction' :: (Member Instruction r, Member LogInstructionFetch r) => Word32 -> InstructionType -> Eff r ()
buildInstruction' _ (Add rd rs1 rs2) = do
    r1 <- readRegister rs1  
    r2 <- readRegister rs2
    writeRegister rd $ r1 + r2
    buildInstruction
buildInstruction' _ (Addi imm rd rs1) = do
    r1 <- readRegister rs1
    writeRegister rd $ r1 + getImmediate imm
    buildInstruction
buildInstruction' _ (Lw imm rd rs1) = do
    r1 <- readRegister rs1
    -- TODO: Alignment handling
    word <- loadWord $ fromIntegral (r1 + getImmediate imm)
    writeRegister rd $ fromIntegral word
    buildInstruction
buildInstruction' _ (Sw imm rs1 rs2) = do
    r1 <- readRegister rs1
    r2 <- readRegister rs2
    -- TODO: Alignment handling
    storeWord (fromIntegral $ r1 + getImmediate imm) $ fromIntegral r2
    buildInstruction
buildInstruction' pc (Blt imm rs1 rs2) = do
    r1 <- readRegister rs1
    r2 <- readRegister rs2
    -- TODO: Alignment handling
    when (r1 < r2) $
        writePC $ fromIntegral $ fromIntegral pc + getImmediate imm
    buildInstruction
buildInstruction' pc (Jal imm rd) = do
    nextInstr <- readPC
    -- TODO: Alignment handling
    writePC $ fromIntegral $ fromIntegral pc + getImmediate imm
    writeRegister rd $ fromIntegral nextInstr
    buildInstruction
buildInstruction' pc (Jalr imm rs1 rd) = do
    nextInstr <- readPC
    rs1Val <- readRegister rs1
    writePC $ fromIntegral (rs1Val + getImmediate imm) .&. 0xfffffffe
    writeRegister rd $ fromIntegral nextInstr
    buildInstruction
buildInstruction' _ (Lui rd imm) = do
    writeRegister rd $ getImmediate imm
    buildInstruction
buildInstruction' pc (Auipc rd imm) = do
    writeRegister rd $ fromIntegral pc + getImmediate imm
    buildInstruction
buildInstruction' _ InvalidInstruction = pure () -- XXX: ignore for now
buildInstruction' _ _ = unexpectedError

buildInstruction :: (Member Instruction r, Member LogInstructionFetch r) => Eff r ()
buildInstruction = do
    -- fetch & decode instruction at current PC
    pc <- readPC
    instrWord <- loadWord pc
    let inst = decode instrWord

    logFetched pc inst

    -- Increment PC before execute', allows setting PC to to
    -- different values in execute' for jumps and branches.
    writePC $ pc + 4

    buildInstruction' pc inst

buildAST :: (Member Instruction r, Member LogInstructionFetch r) => Word32 -> Eff r ()
buildAST entry = writePC entry >> buildInstruction
