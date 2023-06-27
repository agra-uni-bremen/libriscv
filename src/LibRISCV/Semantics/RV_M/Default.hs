{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
module LibRISCV.Semantics.RV_M.Default where

import LibRISCV.Decoder.Opcodes (RV_I(..), RV_M (..), RV32_I (..))
import Control.Monad.Freer
import LibRISCV.Effects.Operations.Language (Operations(..), Size(..), exception, readPC, ecall, ebreak)
import LibRISCV.Effects.Logging.Language (LogInstructionFetch)
import LibRISCV.Effects.Decoding.Language (Decoding, decodeShamt)
import LibRISCV.Effects.Expressions.Language (ExprEval, evalBool)
import Data.Int (Int32)
import LibRISCV.Effects.Expressions.Expr 
import LibRISCV.Semantics.Utils
import Data.BitVector (ones)
import Conversion
import Control.Monad.Extra (whenM, unlessM, ifM)
import Data.Function (on)


instrSemantics :: forall v r . (Member (Operations v) r, Member LogInstructionFetch r, Member (Decoding v) r, Member (ExprEval v) r) => Int -> RV_M -> Eff r ()
instrSemantics width = 
    let
        fromUInt = FromInt width
        mask1 = FromInt width (2^width - 1)
        extract32 = flip Extract 32

        -- False if a given address is not aligned at the four-byte boundary.
        isMisaligned :: Expr v -> Eff r Bool
        isMisaligned addr = evalBool $ (addr `And` fromUInt 0x3) `Uge` fromUInt 1
    in \case 
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
        (r1, r2, rd) <- decodeAndReadRType

        ifM (evalBool $ FromImm r2 `Eq` fromUInt 0) 
            do writeRegister rd mask1
            do ifM (evalBool $ (FromImm r1 `Eq` fromUInt (fromIntegral (minBound :: Int32))) `And` mask1) 
                do writeRegister rd $ FromImm r1
                do writeRegister rd $ FromImm r1 `SDiv` FromImm r2
    DIVU -> do
        (r1, r2, rd) <- decodeAndReadRType
        
        ifM (evalBool $ FromImm r2 `Eq` fromUInt 0) 
            do writeRegister rd mask1
            do writeRegister rd $ FromImm r1 `UDiv` FromImm r2
    REM -> do
        (r1, r2, rd) <- decodeAndReadRType
        ifM (evalBool $ FromImm r2 `Eq` fromUInt 0)
            do writeRegister rd $ FromImm r1
            do ifM (evalBool $ (FromImm r1 `Eq` fromUInt (fromIntegral (minBound :: Int32))) `And` (FromImm r2 `Eq` fromUInt 0xFFFFFFFF)) 
                do writeRegister rd $ fromUInt 0
                do writeRegister rd $ FromImm r1 `SRem` FromImm r2
    REMU -> do
        (r1, r2, rd) <- decodeAndReadRType
        ifM (evalBool $ FromImm r2 `Eq` fromUInt 0) 
            do writeRegister rd $ FromImm r1
            do writeRegister rd $ FromImm r1 `URem` FromImm r2