{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module LibRISCV.Semantics.RV32_I.Default where

import Control.Monad.Freer
import LibRISCV.Effects.Decoding.Language (Decoding, decodeShamt)
import LibRISCV.Effects.Expressions.Expr
import LibRISCV.Effects.Expressions.Language (ExprEval)
import LibRISCV.Effects.Logging.Language (LogInstructionFetch)
import LibRISCV.Effects.Operations.Language (Operations (..))
import LibRISCV.Internal.Decoder.Opcodes (RV32_I (..))
import LibRISCV.Semantics.Utils

instrSemantics :: forall v r. (Member (Operations v) r, Member LogInstructionFetch r, Member (Decoding v) r, Member (ExprEval v) r) => RV32_I -> Eff r ()
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
