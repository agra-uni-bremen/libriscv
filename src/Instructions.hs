{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Instructions (buildAST, InstructionF(..)) where

import Data.Bits
import Types (Address)
import Types
import Decoder
import Data.Word
import Control.Monad (when)
import Control.Monad.Free

import Interpreter.Logging.InstructionFetch
import Common.Coproduct

data InstructionF i r =
    ReadRegister RegIdx (Register -> r) |
    WriteRegister RegIdx Register r |
    LoadWord i (Word32 -> r) |
    StoreWord i Word32 r |
    WritePC Word32 r |
    ReadPC (Word32 -> r) |
    UnexpectedError deriving Functor

--makeFree ''InstructionF

-- those functions are now only working with the fixed Address type. 
-- TODO: add a type class and gtgtgt
readRegister :: (InstructionF Address :<: f) => RegIdx -> Free f Register
readRegister r = inject $ ReadRegister @Address r Pure

writeRegister :: (InstructionF Address :<: f) => RegIdx -> Register -> Free f ()
writeRegister r r' = inject $ WriteRegister @Address r r' $ Pure ()

loadWord :: (InstructionF Address :<: f) => Address -> Free f Word32 
loadWord i = inject $ LoadWord @Address i Pure 

storeWord :: (InstructionF Address :<: f) => Address -> Word32 -> Free f ()
storeWord i w = inject $ StoreWord i w $ Pure ()

writePC :: (InstructionF Address :<: f) => Word32 -> Free f ()
writePC w = inject $ WritePC @Address w $ Pure ()

readPC :: (InstructionF Address :<: f) => Free f Word32
readPC = inject $ ReadPC @Address Pure

unexpectedError :: (InstructionF Address :<: f) => Free f a
unexpectedError = inject $ UnexpectedError @Address

------------------------------------------------------------------------

buildInstruction' :: (InstructionF Address :<: f, LogInstructionFetch :<: f) => Word32 -> Instruction -> Free f ()
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

--buildInstruction :: InstructionM Address ()
buildInstruction :: (LogInstructionFetch :<: f, InstructionF Address :<: f) => Free f ()
buildInstruction = do
    -- fetch & decode instruction at current PC
    pc <- readPC
    instrWord <- loadWord pc
    let inst = decode instrWord

    logFetch pc inst
    
    -- Increment PC before execute', allows setting PC to to
    -- different values in execute' for jumps and branches.
    writePC $ pc + 4

    buildInstruction' pc inst

--buildAST :: (InstructionF Address :<: f, LogInstructionFetch :<: f) => Word32 -> Free f ()
buildAST :: (InstructionF Address :<: f, LogInstructionFetch :<: f) => Word32 -> Free f ()
buildAST entry = writePC entry >> buildInstruction