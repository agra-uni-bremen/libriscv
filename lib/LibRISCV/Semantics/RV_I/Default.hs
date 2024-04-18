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
{-# LANGUAGE LambdaCase #-}

module LibRISCV.Semantics.RV_I.Default where
import LibRISCV.Internal.Decoder.Opcodes (RV_I(..), RV_M (..), RV32_I (..))
import Control.Monad.Freer
import LibRISCV.Effects.Operations.Language (Operations(..), Size(..), exception, readPC, ecall, ebreak)
import LibRISCV.Effects.Logging.Language (LogInstructionFetch)
import LibRISCV.Effects.Decoding.Language (Decoding, decodeShamt)
import LibRISCV.Effects.Expressions.Language (ExprEval, whenExprM, unlessExprM)
import Data.Int (Int32)
import LibRISCV.Effects.Expressions.Expr
import LibRISCV.Semantics.Utils
import Data.BitVector (ones)
import Control.Monad.Extra (whenM)

instrSemantics :: forall v r .
  ( Member (Operations v) r
  , Member LogInstructionFetch r
  , Member (Decoding v) r,
  Member (ExprEval v) r) => Int -> v -> RV_I -> Eff r ()
instrSemantics width pc =
    let
        fromUInt :: Integer -> Expr v
        fromUInt = FromInt width

        extract32 :: Int -> Expr v -> Expr v
        extract32 = flip Extract 32

        -- False if a given address is not aligned at the four-byte boundary.
        isMisaligned :: Expr v -> Expr v
        isMisaligned addr = (addr `And` fromUInt 0x3) `Uge` fromUInt 1
    in \case
        ADDI -> do
            (r1, rd, imm) <- decodeAndReadIType @v
            writeRegister rd $ r1 `addImm` imm
        SLTI -> do
            (r1, rd, imm) <- decodeAndReadIType @v
            let cond = r1 `slt` imm
            writeRegister rd cond
        SLTIU -> do
            (r1, rd, imm) <- decodeAndReadIType @v
            let cond = r1 `ult` imm
            writeRegister rd cond
        ANDI -> do
            (r1, rd, imm) <- decodeAndReadIType @v
            writeRegister rd $ r1 `andImm` imm
        ORI -> do
            (r1, rd, imm) <- decodeAndReadIType @v
            writeRegister rd $ r1 `orImm` imm
        XORI -> do
            (r1, rd, imm) <- decodeAndReadIType @v
            writeRegister rd $ r1 `xorImm` imm
        LUI -> do
            (rd, imm) <- decodeUType @v
            writeRegister rd $ FromImm imm
        AUIPC -> do
            (rd, imm) <- decodeUType
            writeRegister rd $ pc `addImm` imm
        ADD -> do
            (r1, r2, rd) <- decodeAndReadRType @v
            writeRegister rd $ r1 `addImm` r2
        SLT -> do
            (r1, r2, rd) <- decodeAndReadRType
            let cond = r1 `slt` r2 :: Expr v
            writeRegister rd cond
        SLTU -> do
            (r1, r2, rd) <- decodeAndReadRType
            let cond = r1 `ult` r2 :: Expr v
            writeRegister rd cond
        AND -> do
            (r1, r2, rd) <- decodeAndReadRType @v
            writeRegister rd $ r1 `andImm` r2
        OR -> do
            (r1, r2, rd) <- decodeAndReadRType @v
            writeRegister rd $ r1 `orImm` r2
        XOR -> do
            (r1, r2, rd) <- decodeAndReadRType @v
            writeRegister rd $ r1 `xorImm` r2
        SLL -> do
            (r1, r2, rd) <- decodeAndReadRType @v
            writeRegister rd $ FromImm r1 `LShl` regShamt width (FromImm r2)
        SRL -> do
            (r1, r2, rd) <- decodeAndReadRType @v
            writeRegister rd $ FromImm r1 `LShr` regShamt width (FromImm r2)
        SUB -> do
            (r1, r2, rd) <- decodeAndReadRType @v
            writeRegister rd $ r1 `sub` r2
        SRA -> do
            (r1, r2, rd) <- decodeAndReadRType @v
            writeRegister rd $ FromImm r1 `AShr` regShamt width (FromImm r2)
        JAL -> do
            nextInstr <- readPC
            (rd, imm) <- decodeJType

            let newPC = pc `addImm` imm
            writePC newPC
            whenExprM (isMisaligned newPC) $
                exception pc "misaligned PC"
            writeRegister rd (FromImm nextInstr)
        JALR -> do
            nextInstr <- readPC
            (r1, rd, imm) <- decodeAndReadIType

            let newPC = (r1 `addImm` imm) `And` fromUInt 0xfffffffe
            writePC newPC
            whenExprM (isMisaligned newPC) $
                exception pc "misaligned PC"
            writeRegister rd $ FromImm nextInstr
        LB -> do
            (r1, rd, imm) <- decodeAndReadIType @v
            byte <- load Byte $ r1 `addImm` imm
            -- TODO: Alignment handling
            writeRegister rd $ sextImm 24 byte
        LBU -> do
            (r1, rd, imm) <- decodeAndReadIType @v
            -- TODO: Alignment handling
            byte <- load Byte $ r1 `addImm` imm
            writeRegister rd $ zextImm 24 byte
        LH -> do
            (r1, rd, imm) <- decodeAndReadIType @v
            -- TODO: Alignment handling
            half <- load Half $ r1 `addImm` imm
            writeRegister rd $ sextImm 16 half
        LHU -> do
            (r1, rd, imm) <- decodeAndReadIType @v
            -- TODO: Alignment handling
            half <- load Half $ r1 `addImm` imm
            writeRegister rd $ zextImm 16 half
        LW -> do
            (r1, rd, imm) <- decodeAndReadIType @v

            -- TODO: Alignment handling
            word <- load Word $ r1 `addImm` imm
            writeRegister rd $ FromImm word
        SB -> do
            (r1, r2, imm) <- decodeAndReadSType @v

            -- TODO: Alignment handling
            store Byte (r1 `addImm` imm) $ FromImm r2
        SH -> do
            (r1, r2, imm) <- decodeAndReadSType @v

            -- TODO: Alignment handling
            store Half (r1 `addImm` imm) $ FromImm r2
        SW -> do
            (r1, r2, imm) <- decodeAndReadSType @v
            -- TODO: Alignment handling
            store Word (r1 `addImm` imm) $ FromImm r2
        BEQ -> do
            (r1, r2, imm) <- decodeAndReadBType

            let addr = pc `add` imm
            whenExprM (r1 `eq` r2) $ do
                writePC addr
                whenExprM (isMisaligned addr) $
                    exception pc "misaligned PC"
        BNE -> do
            (r1, r2, imm) <- decodeAndReadBType

            let addr = pc `add` imm
            unlessExprM (r1 `eq` r2) $ do
                writePC addr
                whenExprM (isMisaligned addr) $
                    exception pc "misaligned PC"
        BLT -> do
            (r1, r2, imm) <- decodeAndReadBType

            let addr = pc `add` imm
            whenExprM (r1 `slt` r2) $ do
                writePC addr
                whenExprM (isMisaligned addr) $
                    exception pc "misaligned PC"
        BLTU -> do
            (r1, r2, imm) <- decodeAndReadBType

            let addr = pc `add` imm
            whenExprM (r1 `ult` r2) $ do
                writePC @v $ addr
                whenExprM (isMisaligned addr) $
                    exception pc "misaligned PC"
        BGE -> do
            (r1, r2, imm) <- decodeAndReadBType

            let addr = pc `add` imm
            whenExprM (r1 `sge` r2) $ do
                writePC addr
                whenExprM (isMisaligned addr) $
                    exception pc "misaligned PC"
        BGEU -> do
            (r1, r2, imm) <- decodeAndReadBType

            let addr = pc `add` imm
            whenExprM (r1 `uge` r2) $ do
                writePC addr
                whenExprM (isMisaligned addr) $
                    exception pc "misaligned PC"
        FENCE -> pure () -- XXX: ignore for now
        ECALL -> ecall pc
        EBREAK -> ebreak pc
