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

module LibRISCV.Semantics.RV32_I.Default where

import LibRISCV.Decoder.Opcodes (RV_I(..), RV_M (..), RV32_I (..))
import Control.Monad.Freer
import LibRISCV.Effects.Operations.Language (Operations(..))
import LibRISCV.Effects.Logging.Language (LogInstructionFetch)
import LibRISCV.Effects.Decoding.Language (Decoding, decodeShamt)
import LibRISCV.Effects.Expressions.Language (ExprEval)
import Data.Int (Int32)
import LibRISCV.Effects.Expressions.Expr 
import LibRISCV.Semantics.Utils


instrSemantics :: forall v r . (Member (Operations v) r, Member LogInstructionFetch r, Member (Decoding v) r, Member (ExprEval v) r) => RV32_I -> Eff r ()
instrSemantics = \case 
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