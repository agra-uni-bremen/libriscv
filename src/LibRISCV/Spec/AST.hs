{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE BangPatterns #-}
module LibRISCV.Spec.AST where

import LibRISCV.Decoder.Opcode
import Data.Word
import Control.Monad.Freer

import LibRISCV.Effects.Logging.InstructionFetch
import Conversion
import LibRISCV.Spec.Expr
import LibRISCV.Spec.Operations hiding ((>>))
import qualified LibRISCV.Spec.Operations as OP ((>>))
import Control.Applicative (liftA3, Applicative (liftA2))
import Data.Int (Int32)

import Data.Parameterized.NatRepr
import GHC.TypeLits
import Data.BitVector (BV, pow, ones)
import Debug.Trace (trace)
import Data.Function (on)
------------------------------------------------------------------------

-- We require type annotations here to workaround a limitation of
-- GHC type inference in conjunction with freer-simple. Alternatively,
-- we could use a proxy type.
--
-- See: https://github.com/lexi-lambda/freer-simple/issues/7

instrSemantics :: forall v r . (Member (Operations v) r, Show v) => Int -> v -> v -> InstructionType -> Eff r ()
instrSemantics width pc inst ty = let 
        fromImm = FromImm width 
        fromImmHalf = FromImm (div width 2)
        fromImmByte = FromImm 8
        fromUInt = FromUInt width
        mask1 = fromUInt (ones width)

        -- False if a given address is not aligned at the four-byte boundary.
        isMisaligned :: Expr v -> Expr v
        isMisaligned addr = (addr `And` fromUInt 0x3) `Uge` fromUInt 1
    in case ty of
    ADDI -> do
        (r1, rd, imm) <- decodeAndReadIType @v inst
        writeRegister @v rd $ r1 `addSImm` imm
    SLTI -> do
        (r1, rd, imm) <- decodeAndReadIType @v inst
        let cond = fromImm r1 `Slt` fromImm imm :: Expr v
        writeRegister @v rd $ convert cond
    SLTIU -> do
        (r1, rd, imm) <- decodeAndReadIType @v inst
        let cond = fromImm r1 `Ult` fromImm imm :: Expr v
        writeRegister @v rd $ convert cond
    ANDI -> do
        (r1, rd, imm) <- decodeAndReadIType @v inst
        writeRegister @v rd $ r1 `andImm` imm
    ORI -> do
        (r1, rd, imm) <- decodeAndReadIType @v inst
        writeRegister @v rd $ r1 `orImm` imm
    XORI -> do
        (r1, rd, imm) <- decodeAndReadIType @v inst
        writeRegister @v rd $ r1 `xorImm` imm
    SLLI -> do
        (r1, rd, _) <- decodeAndReadIType inst
        shamt <- decodeShamt @v inst
        writeRegister @v rd $ r1 `lshlImm` shamt
    SRLI -> do
        (r1, rd, _) <- decodeAndReadIType inst
        shamt <- decodeShamt @v inst
        writeRegister @v rd $ r1 `lshrImm` shamt
    SRAI -> do
        (r1, rd, _) <- decodeAndReadIType inst
        shamt <- decodeShamt @v inst
        writeRegister @v rd $ r1 `ashrImm` shamt
    LUI -> do
        (rd, imm) <- decodeUType inst
        writeRegister @v rd $ fromImm imm
    AUIPC -> do
        (rd, imm) <- decodeUType inst
        writeRegister @v rd $ pc `addSImm` imm
    ADD -> do
        (r1, r2, rd) <- decodeAndReadRType inst
        writeRegister @v rd $ r1 `addSImm` r2
    SLT -> do
        (r1, r2, rd) <- decodeAndReadRType inst
        let cond = fromImm r1 `Slt` fromImm r2 :: Expr v
        writeRegister @v rd $ convert cond
    SLTU -> do
        (r1, r2, rd) <- decodeAndReadRType inst
        let cond = fromImm r1 `Ult` fromImm r2 :: Expr v
        writeRegister @v rd $ convert cond
    AND -> do
        (r1, r2, rd) <- decodeAndReadRType inst
        writeRegister @v rd $ r1 `andImm` r2
    OR -> do
        (r1, r2, rd) <- decodeAndReadRType inst
        writeRegister @v rd $ r1 `orImm` r2
    XOR -> do
        (r1, r2, rd) <- decodeAndReadRType inst
        writeRegister @v rd $ r1 `xorImm` r2
    SLL -> do
        (r1, r2, rd) <- decodeAndReadRType inst
        writeRegister @v rd $ fromImm r1 `LShl` regShamt width (fromImm r2)
    SRL -> do
        (r1, r2, rd) <- decodeAndReadRType inst
        writeRegister @v rd $ fromImm r1 `LShr` regShamt width (fromImm r2)
    SUB -> do
        (r1, r2, rd) <- decodeAndReadRType inst
        writeRegister @v rd $ fromImm r1 `Sub` fromImm r2
    SRA -> do
        (r1, r2, rd) <- decodeAndReadRType inst
        writeRegister @v rd $ fromImm r1 `AShr` regShamt width (fromImm r2)
    JAL -> do
        nextInstr <- readPC
        (rd, imm) <- decodeJType inst

        let newPC = pc `addSImm` imm
        writePC @v newPC
        runIf (isMisaligned newPC) $
            Exception pc "misaligned PC"
        writeRegister @v rd (fromImm nextInstr)
    JALR -> do
        nextInstr <- readPC
        (r1, rd, imm) <- decodeAndReadIType @v inst

        let newPC = (r1 `addSImm` imm) `And` fromUInt 0xfffffffe
        writePC @v newPC
        runIf (isMisaligned newPC) $
            Exception pc "misaligned PC"
        writeRegister @v rd $ fromImm nextInstr
    LB -> do
        (r1, rd, imm) <- decodeAndReadIType @v inst
        byte <- loadByte @v $ r1 `addSImm` imm
        -- TODO: Alignment handling
        writeRegister @v rd (SExt 8 $ fromImmByte byte)
    LBU -> do
        (r1, rd, imm) <- decodeAndReadIType @v inst
        -- TODO: Alignment handling
        byte <- loadByte @v $ r1 `addSImm` imm
        writeRegister @v rd (ZExt 8 $ fromImmByte byte)
    LH -> do
        (r1, rd, imm) <- decodeAndReadIType @v inst
        -- TODO: Alignment handling
        half <- loadHalf $ r1 `addSImm` imm
        writeRegister @v rd (SExt (div width 2) $ fromImmHalf half)
    LHU -> do
        (r1, rd, imm) <- decodeAndReadIType @v inst
        -- TODO: Alignment handling
        half <- loadHalf $ r1 `addSImm` imm
        writeRegister @v rd (ZExt (div width 2) $ fromImmHalf half)
    LW -> do
        (r1, rd, imm) <- decodeAndReadIType @v inst

        -- TODO: Alignment handling
        word <- loadWord $ r1 `addSImm` imm
        writeRegister @v rd (fromImm word)
    SB -> do
        (r1, r2, imm) <- decodeAndReadSType inst

        -- TODO: Alignment handling
        storeByte @v (r1 `addSImm` imm) $ fromImm r2
    SH -> do
        (r1, r2, imm) <- decodeAndReadSType inst

        -- TODO: Alignment handling
        storeHalf @v (r1 `addSImm` imm) $ fromImm r2
    SW -> do
        (r1, r2, imm) <- decodeAndReadSType inst

        -- TODO: Alignment handling
        storeWord @v (r1 `addSImm` imm) $ fromImm r2
    BEQ -> do
        (r1, r2, imm) <- decodeAndReadBType inst

        -- TODO: Alignment handling
        let cond = fromImm r1 `Eq` fromImm r2
        runIf cond $
            WritePC @v $ fromImm pc `Add` fromImm imm
    BNE -> do
        (r1, r2, imm) <- decodeAndReadBType inst

        -- TODO: Alignment handling
        let cond = fromImm r1 `Eq` fromImm r2
        runUnless cond $
            WritePC @v $ fromImm pc `Add` fromImm imm
    BLT -> do
        (r1, r2, imm) <- decodeAndReadBType inst

        let addr = fromImm pc `Add` fromImm imm
        let cond = fromImm r1 `Slt` fromImm r2
        runIf cond $ OP.do
            WritePC @v $ addr
            RunIf (isMisaligned addr) $
                Exception pc "misaligned PC"
    BLTU -> do
        (r1, r2, imm) <- decodeAndReadBType inst

        let addr = fromImm pc `Add` fromImm imm
        let cond = fromImm r1 `Ult` fromImm r2
        runIf cond $ OP.do
            WritePC @v $ addr
            RunIf (isMisaligned addr) $
                Exception pc "misaligned PC"
    BGE -> do
        (r1, r2, imm) <- decodeAndReadBType inst

        let addr = fromImm pc `Add` fromImm imm
        let cond = fromImm r1 `Sge` fromImm r2
        runIf cond $ OP.do
            WritePC @v $ addr
            RunIf (isMisaligned addr) $
                Exception pc "misaligned PC"
    BGEU -> do
        (r1, r2, imm) <- decodeAndReadBType inst

        let addr = fromImm pc `Add` fromImm imm
        let cond = fromImm r1 `Uge` fromImm r2
        runIf cond $ OP.do 
            WritePC @v $ addr
            RunIf (isMisaligned addr) $
                Exception pc "misaligned PC"
    MUL -> do
        (r1, r2, rd) <- decodeAndReadRType inst
        let 
            multRes = (Mul `on` (SExt 32 . fromImm)) r1 r2
            res = multRes `And` mask1
        writeRegister @v rd res
    MULH -> do
        (r1, r2, rd) <- decodeAndReadRType inst
        let 
            multRes = (Mul `on` (SExt 32 . fromImm)) r1 r2
            res = (multRes `LShr` fromUInt 32) `And` mask1
        writeRegister @v rd res
    MULHU -> do
        (r1, r2, rd) <- decodeAndReadRType inst
        let 
            multRes = (Mul `on` (ZExt 32 . fromImm)) r1 r2
            res = (multRes `LShr` fromUInt 32) `And` mask1
        writeRegister @v rd res
    MULHSU -> do
        (r1, r2, rd) <- decodeAndReadRType inst
        let 
            multRes = (SExt 32 . fromImm) r1 `Mul` (ZExt 32 . fromImm) r2
            res = (multRes `LShr` fromUInt 32) `And` mask1
        writeRegister @v rd res
    DIV -> do
        (r1, r2, rd) <- decodeAndReadRType inst
        let 
            cond  = fromImm r2 `Eq` fromUInt 0
            cond' = (fromImm r1 `Eq` fromUInt (fromIntegral (minBound :: Int32))) `And` mask1
        runIfElse cond 
            do 
                WriteRegister rd mask1
            do 
                RunIfElse cond' 
                    do 
                        WriteRegister rd $ fromImm r1
                    do 
                        WriteRegister @v rd $ fromImm r1 `SDiv` fromImm r2
    DIVU -> do
        (r1, r2, rd) <- decodeAndReadRType inst
        let
            cond = fromImm r2 `Eq` fromUInt 0
        runIfElse cond 
            do 
                WriteRegister rd mask1
            do 
                WriteRegister @v rd $ fromImm r1 `UDiv` fromImm r2
    REM -> do
        (r1, r2, rd) <- decodeAndReadRType inst
        let 
            cond  = fromImm r2 `Eq` fromUInt 0
            cond' = (fromImm r1 `Eq` fromUInt (fromIntegral (minBound :: Int32))) `And` (fromImm r2 `Eq` fromUInt 0xFFFFFFFF)
        runIfElse cond 
            do 
                WriteRegister rd $ fromImm r1
            do 
                RunIfElse cond' 
                    do 
                        WriteRegister rd $ fromUInt 0
                    do 
                        WriteRegister @v rd $ fromImm r1 `SRem` fromImm r2
    REMU -> do
        (r1, r2, rd) <- decodeAndReadRType inst
        let
            cond = fromImm r2 `Eq` fromUInt 0
        runIfElse cond 
            do 
                WriteRegister rd $ fromImm r1
            do
                WriteRegister rd $ fromImm r1 `URem` fromImm r2
    FENCE -> pure () -- XXX: ignore for now
    ECALL -> ecall @v pc
    EBREAK -> ebreak @v pc
    InvalidInstruction -> error "InvalidInstruction"
            

-- TODO add newTypes for type safety
-- decode and read register
decodeAndReadIType :: forall v r . (Member (Operations v) r) => v -> Eff r (v,v,v)
decodeAndReadIType inst = liftA3 (,,) (decodeRS1 inst >>= readRegister) (decodeRD inst) (decodeImmI inst)

-- decode and read register
decodeAndReadBType :: forall v r . (Member (Operations v) r) => v -> Eff r (v,v,v)
decodeAndReadBType inst = liftA3 (,,) (decodeRS1 inst >>= readRegister) (decodeRS2 inst >>= readRegister) (decodeImmB inst)

-- decode and read register
decodeAndReadSType :: forall v r . (Member (Operations v) r) => v -> Eff r (v,v,v)
decodeAndReadSType inst = liftA3 (,,) (decodeRS1 inst >>= readRegister) (decodeRS2 inst >>= readRegister) (decodeImmS inst)

-- decode and read register
decodeAndReadRType :: forall v r . (Member (Operations v) r) => v -> Eff r (v,v,v)
decodeAndReadRType inst = liftA3 (,,) (decodeRS1 inst >>= readRegister) (decodeRS2 inst >>= readRegister) (decodeRD inst)

-- decode and read register
decodeJType :: forall v r . (Member (Operations v) r) => v -> Eff r (v,v)
decodeJType inst = liftA2 (,) (decodeRD inst) (decodeImmJ inst)

decodeUType :: forall v r . (Member (Operations v) r) => v -> Eff r (v,v)
decodeUType inst = liftA2 (,) (decodeRD inst) (decodeImmU inst)

buildInstruction' :: forall v r. (Conversion v Word32, Member (Operations v) r, Member LogInstructionFetch r, Show v) => Int -> v -> v -> InstructionType -> Eff r ()
buildInstruction' _ _ _ InvalidInstruction = pure () -- XXX: ignore for now
buildInstruction' width pc word inst = do
    instrSemantics @v width pc word inst
    buildInstruction @v width

------------------------------------------------------------------------

buildInstruction :: forall v r . (Conversion v Word32, Member (Operations v) r, Member LogInstructionFetch r, Show v) => Int -> Eff r ()
buildInstruction width = do
    -- fetch & decode instruction at current PC
    pc <- readPC @v
    instrWord <- loadWord $ FromImm width pc
    let inst = decode $ convert instrWord

    logFetched (convert pc) inst
    -- Increment PC before execute', allows setting PC to to
    -- different values in execute' for jumps and branches.
    writePC $ FromImm width pc `Add` FromUInt width 4

    buildInstruction' width pc instrWord inst

buildAST :: forall w v r . (KnownNat w, Conversion v Word32, Member (Operations v) r, Member LogInstructionFetch r, Show v) => v -> Eff r ()
buildAST entry = 
    let 
        !width = fromIntegral (intValue (knownNat :: NatRepr w))
    in writePC @v (FromImm width entry) >> buildInstruction @v width
