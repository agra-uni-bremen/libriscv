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
{-# LANGUAGE LambdaCase #-}
module LibRISCV.Semantics.Default where

import LibRISCV.Decoder.Opcode
import Data.Word
import Control.Monad.Freer

import LibRISCV.Effects.Logging.Language ( LogInstructionFetch )
import Conversion
import LibRISCV.Effects.Expressions.Expr
import LibRISCV.Effects.Expressions.Language 
import Control.Applicative (liftA3, Applicative (liftA2))
import Data.Int (Int32)

import Data.Parameterized.NatRepr
import GHC.TypeLits
import Data.BitVector (BV, bitVec, pow, ones)
import Debug.Trace (trace)
import Data.Function (on)
import Data.Data (Proxy(..))
import LibRISCV.Effects.Decoding.Language
import LibRISCV.Effects.Operations.Language hiding (writeRegister, readRegister, writePC, load, store)
import qualified LibRISCV.Effects.Operations.Language as Op
import Control.Monad (when)
import Control.Monad.Extra (whenM, unlessM, ifM)
------------------------------------------------------------------------

-- We require type annotations here to workaround a limitation of
-- GHC type inference in conjunction with freer-simple. Alternatively,
-- we could use a proxy type.
--
-- See: https://github.com/lexi-lambda/freer-simple/issues/7

instrSemantics :: forall v r . (Member (Operations v) r, Member LogInstructionFetch r, Member (Decoding v) r, Member (ExprEval v) r) => Int -> v -> Eff r ()
instrSemantics width pc = do
    ty <- withInstrType @v Proxy id
    case ty of
        InvalidInstruction -> pure ()
        _                  -> exec ty >> buildInstruction @v width
    where
        exec =
            let
                fromUInt = FromInt width
                mask1 = FromInt width (2^width - 1)
                extract32 = flip Extract 32

                -- False if a given address is not aligned at the four-byte boundary.
                isMisaligned :: Expr v -> Eff r Bool
                isMisaligned addr = evalBool $ (addr `And` fromUInt 0x3) `Uge` fromUInt 1
            in \case
                ADDI -> do
                    (r1, rd, imm) <- decodeAndReadIType @v
                    writeRegister rd $ r1 `addSImm` imm
                SLTI -> do
                    (r1, rd, imm) <- decodeAndReadIType @v
                    let cond = FromImm r1 `Slt` FromImm imm
                    writeRegister rd $ convert cond
                SLTIU -> do
                    (r1, rd, imm) <- decodeAndReadIType @v
                    let cond = FromImm r1 `Ult` FromImm imm
                    writeRegister rd $ convert cond
                ANDI -> do
                    (r1, rd, imm) <- decodeAndReadIType @v
                    writeRegister rd $ r1 `andImm` imm
                ORI -> do
                    (r1, rd, imm) <- decodeAndReadIType @v
                    writeRegister rd $ r1 `orImm` imm
                XORI -> do
                    (r1, rd, imm) <- decodeAndReadIType @v
                    writeRegister rd $ r1 `xorImm` imm
                SLLI -> do
                    (r1, rd, _) <- decodeAndReadIType @v
                    shamt <- decodeShamt
                    writeRegister rd $ r1 `lshlImm` shamt
                SRLI -> do
                    (r1, rd, _) <- decodeAndReadIType @v
                    shamt <- decodeShamt
                    writeRegister rd $ r1 `lshrImm` shamt
                SRAI -> do
                    (r1, rd, _) <- decodeAndReadIType @v
                    shamt <- decodeShamt
                    writeRegister rd $ r1 `ashrImm` shamt
                LUI -> do
                    (rd, imm) <- decodeUType @v
                    writeRegister rd $ FromImm imm
                AUIPC -> do
                    (rd, imm) <- decodeUType
                    writeRegister rd $ pc `addSImm` imm
                ADD -> do
                    (r1, r2, rd) <- decodeAndReadRType @v
                    writeRegister rd $ r1 `addSImm` r2
                SLT -> do
                    (r1, r2, rd) <- decodeAndReadRType
                    let cond = FromImm r1 `Slt` FromImm r2 :: Expr v
                    writeRegister rd $ convert cond
                SLTU -> do
                    (r1, r2, rd) <- decodeAndReadRType
                    let cond = FromImm r1 `Ult` FromImm r2 :: Expr v
                    writeRegister rd $ convert cond
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
                    writeRegister rd $ FromImm r1 `Sub` FromImm r2
                SRA -> do
                    (r1, r2, rd) <- decodeAndReadRType @v
                    writeRegister rd $ FromImm r1 `AShr` regShamt width (FromImm r2)
                JAL -> do
                    nextInstr <- readPC
                    (rd, imm) <- decodeJType

                    let newPC = pc `addSImm` imm
                    writePC newPC
                    whenM (isMisaligned newPC) $
                        exception pc "misaligned PC"
                    writeRegister rd (FromImm nextInstr)
                JALR -> do
                    nextInstr <- readPC
                    (r1, rd, imm) <- decodeAndReadIType

                    let newPC = (r1 `addSImm` imm) `And` fromUInt 0xfffffffe
                    writePC newPC
                    whenM (isMisaligned newPC) $ 
                        exception pc "misaligned PC"
                    writeRegister rd $ FromImm nextInstr
                LB -> do
                    (r1, rd, imm) <- decodeAndReadIType @v
                    byte <- load Byte $ r1 `addSImm` imm
                    -- TODO: Alignment handling
                    writeRegister rd (SExt 8 $ FromImm byte)
                LBU -> do
                    (r1, rd, imm) <- decodeAndReadIType @v
                    -- TODO: Alignment handling
                    byte <- load Byte $ r1 `addSImm` imm
                    writeRegister rd (ZExt 8 $ FromImm byte)
                LH -> do
                    (r1, rd, imm) <- decodeAndReadIType @v
                    -- TODO: Alignment handling
                    half <- load Half $ r1 `addSImm` imm
                    writeRegister rd (SExt 16 $ FromImm half)
                LHU -> do
                    (r1, rd, imm) <- decodeAndReadIType @v
                    -- TODO: Alignment handling
                    half <- load Half $ r1 `addSImm` imm
                    writeRegister rd (ZExt 16 $ FromImm half)
                LW -> do
                    (r1, rd, imm) <- decodeAndReadIType

                    -- TODO: Alignment handling
                    word <- load @v Word $ r1 `addSImm` imm
                    writeRegister rd (FromImm word)
                SB -> do
                    (r1, r2, imm) <- decodeAndReadSType

                    -- TODO: Alignment handling
                    store @v Byte (r1 `addSImm` imm) $ FromImm r2
                SH -> do
                    (r1, r2, imm) <- decodeAndReadSType

                    -- TODO: Alignment handling
                    store @v Half (r1 `addSImm` imm) $ FromImm r2
                SW -> do
                    (r1, r2, imm) <- decodeAndReadSType @v
                    -- TODO: Alignment handling
                    store Word (r1 `addSImm` imm) $ FromImm r2
                BEQ -> do
                    (r1, r2, imm) <- decodeAndReadBType

                    -- TODO: Alignment handling
                    whenM (evalBool $ FromImm r1 `Eq` FromImm r2) $ do
                        writePC $ FromImm pc `Add` FromImm imm
                BNE -> do
                    (r1, r2, imm) <- decodeAndReadBType

                    -- TODO: Alignment handling
                    unlessM (evalBool $ FromImm r1 `Eq` FromImm r2) $ do
                        writePC $ FromImm pc `Add` FromImm imm
                BLT -> do
                    (r1, r2, imm) <- decodeAndReadBType

                    let addr = FromImm pc `Add` FromImm imm
                    whenM (evalBool $ FromImm r1 `Slt` FromImm r2) $ do
                        writePC addr
                        whenM (isMisaligned addr) $
                            exception pc "misaligned PC"
                BLTU -> do
                    (r1, r2, imm) <- decodeAndReadBType

                    let addr = FromImm pc `Add` FromImm imm
                    whenM (evalBool $ FromImm r1 `Ult` FromImm r2) $ do
                        writePC @v $ addr
                        whenM (isMisaligned addr) $
                            exception pc "misaligned PC"
                BGE -> do
                    (r1, r2, imm) <- decodeAndReadBType

                    let addr = FromImm pc `Add` FromImm imm
                    whenM (evalBool $ FromImm r1 `Sge` FromImm r2) $ do
                        writePC addr
                        whenM (isMisaligned addr) $
                            exception pc "misaligned PC"
                BGEU -> do
                    (r1, r2, imm) <- decodeAndReadBType

                    let addr = FromImm pc `Add` FromImm imm
                    whenM (evalBool $ FromImm r1 `Uge` FromImm r2) $ do
                        writePC addr
                        whenM (isMisaligned addr) $
                            exception pc "misaligned PC"
                MUL -> do
                    (r1, r2, rd) <- decodeAndReadRType @v
                    let
                        multRes = (Mul `on` (SExt 32 . FromImm)) r1 r2
                        res = extract32 0 multRes
                    writeRegister rd res
                MULH -> do
                    (r1, r2, rd) <- decodeAndReadRType @v
                    let
                        multRes = (Mul `on` (SExt 32 . FromImm)) r1 r2
                        res = extract32 32 multRes
                    writeRegister rd res
                MULHU -> do
                    (r1, r2, rd) <- decodeAndReadRType @v
                    let
                        multRes = (Mul `on` (ZExt 32 . FromImm)) r1 r2
                        res = extract32 32 multRes
                    writeRegister rd res
                MULHSU -> do
                    (r1, r2, rd) <- decodeAndReadRType @v
                    let
                        multRes = (SExt 32 . FromImm) r1 `Mul` (ZExt 32 . FromImm) r2
                        res = extract32 32 multRes
                    writeRegister rd res
                DIV -> do
                    (r1, r2, rd) <- decodeAndReadRType @v

                    ifM (evalBool $ FromImm r2 `Eq` fromUInt 0)
                        do writeRegister rd mask1
                        do ifM (evalBool $ (FromImm r1 `Eq` fromUInt (fromIntegral (minBound :: Int32))) `And` mask1)
                            do writeRegister rd $ FromImm r1
                            do writeRegister rd $ FromImm r1 `SDiv` FromImm r2
                DIVU -> do
                    (r1, r2, rd) <- decodeAndReadRType @v

                    ifM (evalBool $ FromImm r2 `Eq` fromUInt 0)
                        do writeRegister rd mask1
                        do writeRegister rd $ FromImm r1 `UDiv` FromImm r2
                REM -> do
                    (r1, r2, rd) <- decodeAndReadRType @v
                    ifM (evalBool $ FromImm r2 `Eq` fromUInt 0)
                        do writeRegister rd $ FromImm r1
                        do ifM (evalBool $ (FromImm r1 `Eq` fromUInt (fromIntegral (minBound :: Int32))) `And` (FromImm r2 `Eq` fromUInt 0xFFFFFFFF))
                            do writeRegister rd $ fromUInt 0
                            do writeRegister rd $ FromImm r1 `SRem` FromImm r2
                REMU -> do
                    (r1, r2, rd) <- decodeAndReadRType @v
                    ifM (evalBool $ FromImm r2 `Eq` fromUInt 0)
                        do writeRegister rd $ FromImm r1
                        do writeRegister rd $ FromImm r1 `URem` FromImm r2
                FENCE -> pure () -- XXX: ignore for now
                ECALL -> ecall pc
                EBREAK -> ebreak pc
                InvalidInstruction -> error "InvalidInstruction"


-- TODO add newTypes for type safety
-- decode and read register
decodeAndReadIType :: forall v r . (Member (Decoding v) r, Member (Operations v) r) => Eff r (v,v,v)
decodeAndReadIType = liftA3 (,,) (decodeRS1 >>= Op.readRegister) decodeRD decodeImmI

-- decode and read register
decodeAndReadBType :: forall v r . (Member (Decoding v) r, Member (Operations v) r) => Eff r (v,v,v)
decodeAndReadBType = liftA3 (,,) (decodeRS1 >>= Op.readRegister) (decodeRS2 >>= Op.readRegister) decodeImmB

-- decode and read register
decodeAndReadSType :: forall v r . (Member (Decoding v) r, Member (Operations v) r) => Eff r (v,v,v)
decodeAndReadSType = liftA3 (,,) (decodeRS1 >>= Op.readRegister) (decodeRS2 >>= Op.readRegister) decodeImmS

-- decode and read register
decodeAndReadRType :: forall v r . (Member (Decoding v) r, Member (Operations v) r) => Eff r (v,v,v)
decodeAndReadRType = liftA3 (,,) (decodeRS1 >>= Op.readRegister) (decodeRS2 >>= Op.readRegister) decodeRD

-- decode and read register
decodeJType :: forall v r . (Member (Decoding v) r, Member (Operations v) r) => Eff r (v,v)
decodeJType = liftA2 (,) decodeRD decodeImmJ

decodeUType :: forall v r . (Member (Decoding v) r, Member (Operations v) r) => Eff r (v,v)
decodeUType = liftA2 (,) decodeRD decodeImmU

writeRegister :: forall v r . (Member (ExprEval v) r, Member (Operations v) r) => v -> Expr v -> Eff r ()
writeRegister reg e = eval e >>= Op.writeRegister reg

readRegister :: forall v r . (Member (ExprEval v) r, Member (Operations v) r) => Expr v -> Eff r v
readRegister e = eval e >>= Op.readRegister 

writePC :: forall v r . (Member (ExprEval v) r, Member (Operations v) r) => Expr v -> Eff r ()
writePC e = eval e >>= Op.writePC

load :: forall v r . (Member (ExprEval v) r, Member (Operations v) r) => Size -> Expr v -> Eff r v
load s e = eval e >>= Op.load s

store :: forall v r . (Member (ExprEval v) r, Member (Operations v) r) => Size -> Expr v -> Expr v -> Eff r ()
store s r e = do
    reg <- eval r
    v   <- eval e
    Op.store s reg v

------------------------------------------------------------------------

buildInstruction :: forall v r . (Member (Operations v) r, Member LogInstructionFetch r, Member (Decoding v) r, Member (ExprEval v) r) => Int -> Eff r ()
buildInstruction width = do
    -- fetch instruction at current PC
    pc <- readPC @v
    instrWord <- load @v Word $ FromImm pc
    setInstr instrWord

    -- Increment PC before execute', allows setting PC to to
    -- different values in execute' for jumps and branches.
    writePC $ FromImm pc `Add` (FromInt width 4)
    instrSemantics width pc

buildAST :: forall w v r . (KnownNat w, Member (Operations v) r, Member LogInstructionFetch r, Member (Decoding v) r, Member (ExprEval v) r) => v -> Eff r ()
buildAST entry =
    let
        !width = fromIntegral (intValue (knownNat :: NatRepr w))
    in writePC (FromImm entry) >> buildInstruction @v width
