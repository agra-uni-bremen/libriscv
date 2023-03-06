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
module LibRISCV.Spec.AST where

import LibRISCV.Decoder.Opcode
import Data.Word
import Control.Monad.Freer

import LibRISCV.Utils (whenMword,unlessMword)
import LibRISCV.Effects.Logging.InstructionFetch
import Conversion
import LibRISCV.Spec.Expr
import LibRISCV.Spec.Operations
import Control.Applicative (liftA3, Applicative (liftA2))

------------------------------------------------------------------------

-- We require type annotations here to workaround a limitation of
-- GHC type inference in conjunction with freer-simple. Alternatively,
-- we could use a proxy type.
--
-- See: https://github.com/lexi-lambda/freer-simple/issues/7

instrSemantics :: forall v r. (Member (Operations v) r, Conversion v Word32) => v -> v -> InstructionType -> Eff r ()
instrSemantics _ inst ADDI = do
    (r1, rd, imm) <- decodeAndReadIType @v inst
    writeRegister @v rd $ r1 `addSImm` imm
instrSemantics _ inst SLTI = do
    (r1, rd, imm) <- decodeAndReadIType @v inst
    let cond = FromImm r1 `Slt` FromImm imm :: Expr v
    writeRegister @v rd $ convert cond
instrSemantics _ inst SLTIU = do
    (r1, rd, imm) <- decodeAndReadIType @v inst
    let cond = FromImm r1 `Ult` FromImm imm :: Expr v
    writeRegister @v rd $ convert cond
instrSemantics _ inst ANDI = do
    (r1, rd, imm) <- decodeAndReadIType @v inst
    writeRegister @v rd $ r1 `andImm` imm
instrSemantics _ inst ORI = do
    (r1, rd, imm) <- decodeAndReadIType @v inst
    writeRegister @v rd $ r1 `orImm` imm
instrSemantics _ inst XORI = do
    (r1, rd, imm) <- decodeAndReadIType @v inst
    writeRegister @v rd $ r1 `xorImm` imm
instrSemantics _ inst SLLI = do
    (r1, rd, _) <- decodeAndReadIType inst
    shamt <- decodeShamt @v inst
    writeRegister @v rd $ r1 `lshlImm` shamt
instrSemantics _ inst SRLI = do
    (r1, rd, _) <- decodeAndReadIType inst
    shamt <- decodeShamt @v inst
    writeRegister @v rd $ r1 `lshrImm` shamt
instrSemantics _ inst SRAI = do
    (r1, rd, _) <- decodeAndReadIType inst
    shamt <- decodeShamt @v inst
    writeRegister @v rd $ r1 `ashrImm` shamt
instrSemantics _ inst LUI = do
    (rd, imm) <- decodeUType inst
    writeRegister @v rd $ FromImm imm
instrSemantics pc inst AUIPC = do
    (rd, imm) <- decodeUType inst
    writeRegister @v rd $ pc `addSImm` imm
instrSemantics _ inst ADD = do
    (r1, r2, rd) <- decodeAndReadRType inst
    writeRegister @v rd $ r1 `addSImm` r2
instrSemantics _ inst SLT = do
    (r1, r2, rd) <- decodeAndReadRType inst
    let cond = FromImm r1 `Slt` FromImm r2 :: Expr v
    writeRegister @v rd $ convert cond
instrSemantics _ inst SLTU = do
    (r1, r2, rd) <- decodeAndReadRType inst
    let cond = FromImm r1 `Ult` FromImm r2 :: Expr v
    writeRegister @v rd $ convert cond
instrSemantics _ inst AND = do
    (r1, r2, rd) <- decodeAndReadRType inst
    writeRegister @v rd $ r1 `andImm` r2
instrSemantics _ inst OR = do
    (r1, r2, rd) <- decodeAndReadRType inst
    writeRegister @v rd $ r1 `orImm` r2
instrSemantics _ inst XOR = do
    (r1, r2, rd) <- decodeAndReadRType inst
    writeRegister @v rd $ r1 `xorImm` r2
instrSemantics _ inst SLL = do
    (r1, r2, rd) <- decodeAndReadRType inst
    writeRegister @v rd $ FromImm r1 `LShl` regShamt (FromImm r2)
instrSemantics _ inst SRL = do
    (r1, r2, rd) <- decodeAndReadRType inst
    writeRegister @v rd $ FromImm r1 `LShr` regShamt (FromImm r2)
instrSemantics _ inst SUB = do
    (r1, r2, rd) <- decodeAndReadRType inst
    writeRegister @v rd $ FromImm r1 `Sub` FromImm r2
instrSemantics _ inst SRA = do
    (r1, r2, rd) <- decodeAndReadRType inst
    writeRegister @v rd $ FromImm r1 `AShr` regShamt (FromImm r2)
instrSemantics pc inst JAL = do
    nextInstr <- readPC
    (rd, imm) <- decodeJType inst
    writePC @v $ pc `addSImm` imm
    writeRegister @v rd (FromImm nextInstr)
instrSemantics _ inst JALR = do
    nextInstr <- readPC
    (r1, rd, imm) <- decodeAndReadIType @v inst
    writePC @v $ (r1 `addSImm` imm) `And` FromUInt 0xfffffffe
    writeRegister @v rd $ FromImm nextInstr
instrSemantics _ inst LB = do
    (r1, rd, imm) <- decodeAndReadIType @v inst
    byte <- loadByte @v $ r1 `addSImm` imm
    -- TODO: Alignment handling
    writeRegister @v rd (SExtByte $ FromImm byte)
instrSemantics _ inst LBU = do
    (r1, rd, imm) <- decodeAndReadIType @v inst
    -- TODO: Alignment handling
    byte <- loadByte @v $ r1 `addSImm` imm
    writeRegister @v rd (ZExtByte $ FromImm byte)
instrSemantics _ inst LH = do
    (r1, rd, imm) <- decodeAndReadIType @v inst
    -- TODO: Alignment handling
    half <- loadHalf $ r1 `addSImm` imm
    writeRegister @v rd (SExtHalf $ FromImm half)
instrSemantics _ inst LHU = do
    (r1, rd, imm) <- decodeAndReadIType @v inst
    -- TODO: Alignment handling
    half <- loadHalf $ r1 `addSImm` imm
    writeRegister @v rd (ZExtHalf $ FromImm half)
instrSemantics _ inst LW = do
    (r1, rd, imm) <- decodeAndReadIType @v inst

    -- TODO: Alignment handling
    word <- loadWord $ r1 `addSImm` imm
    writeRegister @v rd (FromImm word)
instrSemantics _ inst SB = do
    (r1, r2, imm) <- decodeAndReadSType inst

    -- TODO: Alignment handling
    storeByte @v (r1 `addSImm` imm) $ FromImm r2
instrSemantics _ inst SH = do
    (r1, r2, imm) <- decodeAndReadSType inst

    -- TODO: Alignment handling
    storeHalf @v (r1 `addSImm` imm) $ FromImm r2
instrSemantics _ inst SW = do
    (r1, r2, imm) <- decodeAndReadSType inst

    -- TODO: Alignment handling
    storeWord @v (r1 `addSImm` imm) $ FromImm r2
instrSemantics pc inst BEQ = do
    (r1, r2, imm) <- decodeAndReadBType inst

    -- TODO: Alignment handling
    let cond = FromImm r1 `Eq` FromImm r2
    whenMword (convert @v <$> liftE cond) $
        writePC @v $ FromImm pc `Add` FromImm imm
instrSemantics pc inst BNE = do
    (r1, r2, imm) <- decodeAndReadBType inst

    -- TODO: Alignment handling
    let cond = FromImm r1 `Eq` FromImm r2
    unlessMword (convert @v <$> liftE cond) $
        writePC @v $ FromImm pc `Add` FromImm imm
instrSemantics pc inst BLT = do
    (r1, r2, imm) <- decodeAndReadBType inst

    -- TODO: Alignment handling
    let cond = FromImm r1 `Slt` FromImm r2
    whenMword (convert @v <$> liftE cond) $
        writePC @v $ FromImm pc `Add` FromImm imm
instrSemantics pc inst BLTU = do
    (r1, r2, imm) <- decodeAndReadBType inst

    -- TODO: Alignment handling
    let cond = FromImm r1 `Ult` FromImm r2
    whenMword (convert @v <$> liftE cond) $
        writePC @v $ FromImm pc `Add` FromImm imm
instrSemantics pc inst BGE = do
    (r1, r2, imm) <- decodeAndReadBType inst

    -- TODO: Alignment handling
    let cond = FromImm r1 `Sge` FromImm r2
    whenMword (convert @v <$> liftE cond) $
        writePC @v $ FromImm pc `Add` FromImm imm
instrSemantics pc inst BGEU = do
    (r1, r2, imm) <- decodeAndReadBType inst

    -- TODO: Alignment handling
    let cond = FromImm r1 `Uge` FromImm r2
    whenMword (convert @v <$> liftE cond) $
        writePC @v $ FromImm pc `Add` FromImm imm
instrSemantics _ _ FENCE = pure () -- XXX: ignore for now
instrSemantics pc _ ECALL = ecall @v pc
instrSemantics pc __  EBREAK = ebreak @v pc
instrSemantics _ _ _ = error "InvalidInstruction"

-- TODO add newTypes for type safety
-- decode and read register
decodeAndReadIType :: forall v r . (Conversion v Word32, Member (Operations v) r) => v -> Eff r (v,v,v)
decodeAndReadIType inst = liftA3 (,,) (decodeRS1 inst >>= readRegister) (decodeRD inst) (decodeImmI inst)

-- decode and read register
decodeAndReadBType :: forall v r . (Conversion v Word32, Member (Operations v) r) => v -> Eff r (v,v,v)
decodeAndReadBType inst = liftA3 (,,) (decodeRS1 inst >>= readRegister) (decodeRS2 inst >>= readRegister) (decodeImmB inst)

-- decode and read register
decodeAndReadSType :: forall v r . (Conversion v Word32, Member (Operations v) r) => v -> Eff r (v,v,v)
decodeAndReadSType inst = liftA3 (,,) (decodeRS1 inst >>= readRegister) (decodeRS2 inst >>= readRegister) (decodeImmS inst)

-- decode and read register
decodeAndReadRType :: forall v r . (Conversion v Word32, Member (Operations v) r) => v -> Eff r (v,v,v)
decodeAndReadRType inst = liftA3 (,,) (decodeRS1 inst >>= readRegister) (decodeRS2 inst >>= readRegister) (decodeRD inst)

-- decode and read register
decodeJType :: forall v r . (Conversion v Word32, Member (Operations v) r) => v -> Eff r (v,v)
decodeJType inst = liftA2 (,) (decodeRD inst) (decodeImmJ inst)

decodeUType :: forall v r . (Conversion v Word32, Member (Operations v) r) => v -> Eff r (v,v)
decodeUType inst = liftA2 (,) (decodeRD inst) (decodeImmU inst)

buildInstruction' :: forall v r. (Conversion v Word32, Member (Operations v) r, Member LogInstructionFetch r) => v -> v -> InstructionType -> Eff r ()
buildInstruction' _ _ InvalidInstruction = pure () -- XXX: ignore for now
buildInstruction' pc word inst = do
    instrSemantics @v pc word inst
    buildInstruction @v

------------------------------------------------------------------------

buildInstruction :: forall v r . (Conversion v Word32, Member (Operations v) r, Member LogInstructionFetch r) => Eff r ()
buildInstruction = do
    -- fetch & decode instruction at current PC
    pc <- readPC @v
    instrWord <- loadWord $ FromImm pc
    let inst = decode $ convert instrWord

    logFetched (convert pc) inst

    -- Increment PC before execute', allows setting PC to to
    -- different values in execute' for jumps and branches.
    writePC $ FromImm pc `Add` FromUInt 4

    buildInstruction' pc instrWord inst

buildAST :: forall v r . (Conversion v Word32, Member (Operations v) r, Member LogInstructionFetch r) => v -> Eff r ()
buildAST entry = writePC @v (FromImm entry) >> buildInstruction @v
