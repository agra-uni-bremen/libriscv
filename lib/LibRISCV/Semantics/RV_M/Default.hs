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
import LibRISCV.Effects.Expressions.Language (ExprEval, ifExprM)
import Data.Int (Int32)
import LibRISCV.Effects.Expressions.Expr
import LibRISCV.Semantics.Utils
import Data.BitVector (ones)
import Control.Monad.Extra (whenM, ifM)
import Data.Function (on)

instrSemantics :: forall v r .
  ( Member (Operations v) r
  , Member LogInstructionFetch r
  , Member (Decoding v) r
  , Member (ExprEval v) r) => Int -> RV_M -> Eff r ()
instrSemantics width =
    let
        fromUInt :: Integer -> Expr v
        fromUInt = FromInt width

        immEqInt :: v -> Integer -> Expr v
        immEqInt imm int = FromImm imm `Eq` fromUInt int

        mask1 :: Expr v
        mask1 = FromInt width (2^width - 1)

        extract32 :: Int -> Expr v -> Expr v
        extract32 = flip Extract 32

        mostNegative :: Integer
        mostNegative = 2^(width - 1)

        -- Signed division overflow occurs when the most-negative integer is divided by -1.
        sdivOverflow :: v -> v -> Expr v
        sdivOverflow n divisor = (n `immEqInt` mostNegative) `And` (divisor `immEqInt` (-1))
    in \case
    MUL -> do
        (r1, r2, rd) <- decodeAndReadRType
        let
            multRes = (Mul `on` sextImm 32) r1 r2
            res = extract32 0 multRes
        writeRegister rd res
    MULH -> do
        (r1, r2, rd) <- decodeAndReadRType
        let
            multRes = (Mul `on` sextImm 32) r1 r2
            res = extract32 32 multRes
        writeRegister rd res
    MULHU -> do
        (r1, r2, rd) <- decodeAndReadRType
        let
            multRes = (Mul `on` zextImm 32) r1 r2
            res = extract32 32 multRes
        writeRegister rd res
    MULHSU -> do
        (r1, r2, rd) <- decodeAndReadRType
        let
            multRes = sextImm 32 r1 `Mul` zextImm 32 r2
            res = extract32 32 multRes
        writeRegister rd res
    DIV -> do
        (r1, r2, rd) <- decodeAndReadRType
        ifExprM (r2 `immEqInt` 0)
            do writeRegister rd mask1
            do ifExprM (sdivOverflow r1 r2)
                do writeRegister rd $ FromImm r1
                do writeRegister rd $ r1 `sdiv` r2
    DIVU -> do
        (r1, r2, rd) <- decodeAndReadRType
        ifExprM (r2 `immEqInt` 0)
            do writeRegister rd mask1
            do writeRegister rd $ r1 `udiv` r2
    REM -> do
        (r1, r2, rd) <- decodeAndReadRType
        ifExprM (r2 `immEqInt` 0)
            do writeRegister rd $ FromImm r1
            do ifExprM (sdivOverflow r1 r2)
                do writeRegister rd $ fromUInt 0
                do writeRegister rd $ r1 `srem` r2
    REMU -> do
        (r1, r2, rd) <- decodeAndReadRType
        ifExprM (r2 `immEqInt` 0)
            do writeRegister rd $ FromImm r1
            do writeRegister rd $ r1 `urem` r2
