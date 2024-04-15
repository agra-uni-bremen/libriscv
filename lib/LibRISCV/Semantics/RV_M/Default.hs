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

import LibRISCV.Internal.Decoder.Opcodes (RV_I(..), RV_M (..), RV32_I (..))
import Control.Monad.Freer
import LibRISCV.Effects.Operations.Language (Operations(..), Size(..), exception, readPC, ecall, ebreak)
import LibRISCV.Effects.Logging.Language (LogInstructionFetch)
import LibRISCV.Effects.Decoding.Language (Decoding, decodeShamt)
import LibRISCV.Effects.Expressions.Language (ExprEval, isTrue, isFalse)
import Data.Int (Int32)
import LibRISCV.Effects.Expressions.Expr 
import LibRISCV.Semantics.Utils
import Data.BitVector (ones)
import Control.Monad.Extra (whenM, ifM)
import Data.Function (on)

instrSemantics :: forall v r . (Member (Operations v) r, Member LogInstructionFetch r, Member (Decoding v) r, Member (ExprEval v) r) => Int -> RV_M -> Eff r ()
instrSemantics width = 
    let
        fromUInt = FromInt width
        mask1 = FromInt width (2^width - 1)
        extract32 = flip Extract 32

        -- False if a given address is not aligned at the four-byte boundary.
        isMisaligned :: Expr v -> Eff r Bool
        isMisaligned addr = isTrue $ (addr `And` fromUInt 0x3) `Uge` fromUInt 1
    in \case 
    MUL -> do
        (r1, r2, rd) <- decodeAndReadRType @v
        let
            multRes = (Mul `on` (sextImm 32)) r1 r2
            res = extract32 0 multRes
        writeRegister rd res
    MULH -> do
        (r1, r2, rd) <- decodeAndReadRType @v
        let
            multRes = (Mul `on` (sextImm 32)) r1 r2
            res = extract32 32 multRes
        writeRegister rd res
    MULHU -> do
        (r1, r2, rd) <- decodeAndReadRType @v
        let
            multRes = (Mul `on` (zextImm 32)) r1 r2
            res = extract32 32 multRes
        writeRegister rd res
    MULHSU -> do
        (r1, r2, rd) <- decodeAndReadRType @v
        let
            multRes = (sextImm 32) r1 `Mul` (zextImm 32) r2
            res = extract32 32 multRes
        writeRegister rd res
    DIV -> do
        (r1, r2, rd) <- decodeAndReadRType

        ifM (isTrue $ FromImm r2 `Eq` fromUInt 0) 
            do writeRegister rd mask1
            do ifM (isTrue $ (FromImm r1 `Eq` fromUInt (fromIntegral (minBound :: Int32))) `And` mask1) 
                do writeRegister rd $ FromImm r1
                do writeRegister rd $ r1 `sdiv` r2
    DIVU -> do
        (r1, r2, rd) <- decodeAndReadRType
        ifM (isTrue $ FromImm r2 `Eq` fromUInt 0) 
            do writeRegister rd mask1
            do writeRegister rd $ r1 `udiv` r2
    REM -> do
        (r1, r2, rd) <- decodeAndReadRType
        ifM (isTrue $ FromImm r2 `Eq` fromUInt 0)
            do writeRegister rd $ FromImm r1
            do ifM (isTrue $ (FromImm r1 `Eq` fromUInt (fromIntegral (minBound :: Int32))) `And` (FromImm r2 `Eq` fromUInt 0xFFFFFFFF)) 
                do writeRegister rd $ fromUInt 0
                do writeRegister rd $ r1 `srem` r2
    REMU -> do
        (r1, r2, rd) <- decodeAndReadRType
        ifM (isTrue $ FromImm r2 `Eq` fromUInt 0) 
            do writeRegister rd $ FromImm r1
            do writeRegister rd $ r1 `urem` r2
