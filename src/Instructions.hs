{-# LANGUAGE DeriveFunctor, TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Instructions where

import Data.Bits
import Types (Address)
import Types
import Decoder
import Data.Word
import Control.Monad (when)
import Control.Monad.Free
import Control.Monad.Free.TH

data InstructionF i r =
    ReadRegister RegIdx (Register -> r) |
    WriteRegister RegIdx Register r |
    LoadWord i (Word32 -> r) |
    StoreWord i Word32 r |
    WritePC Word32 r |
    ReadPC (Word32 -> r) |
    UnexpectedError deriving Functor

makeFree ''InstructionF

type InstructionM i = Free (InstructionF i)

------------------------------------------------------------------------

buildInstruction' :: Address -> Instruction -> InstructionM Address ()
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
buildInstruction' _ InvalidInstruction = Pure () -- XXX: ignore for now
buildInstruction' _ _ = unexpectedError

buildInstruction :: InstructionM Address ()
buildInstruction = do
    -- fetch & decode instruction at current PC
    pc <- readPC
    instrWord <- loadWord pc
    let inst = decode instrWord

    -- Increment PC before execute', allows setting PC to to
    -- different values in execute' for jumps and branches.
    writePC $ pc + 4

    buildInstruction' pc inst

buildAST :: InstructionM Address ()
buildAST = buildInstruction
