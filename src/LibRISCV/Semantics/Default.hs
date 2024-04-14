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
module LibRISCV.Semantics.Default where

import LibRISCV.Decoder.Opcodes
import Data.Word
import Control.Monad.Freer

import LibRISCV.Effects.Logging.Language ( LogInstructionFetch, logFetched )
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
import LibRISCV.Semantics.Utils
import qualified LibRISCV.Semantics.RV_I.Default as RV_I
import qualified LibRISCV.Semantics.RV32_I.Default as RV32_I
import qualified LibRISCV.Semantics.RV_M.Default as RV_M
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
    logFetched ty
    case ty of
        RV_I inst   -> RV_I.instrSemantics width pc inst >> buildInstruction @v width
        RV32_I inst -> RV32_I.instrSemantics @v inst >> buildInstruction @v width
        RV_M inst   -> RV_M.instrSemantics @v width inst >> buildInstruction @v width
        InvalidInstruction -> pure ()

buildInstruction :: forall v r . (Member (Operations v) r, Member LogInstructionFetch r, Member (Decoding v) r, Member (ExprEval v) r) => Int -> Eff r ()
buildInstruction width = do
    -- fetch instruction at current PC
    pc <- readPC @v
    instrWord <- load @v Word $ FromImm pc
    setInstr instrWord

    -- Increment PC before execute', allows setting PC to to
    -- different values in execute' for jumps and branches.
    writePC $ FromImm pc `Add` FromInt width 4
    instrSemantics width pc

buildAST :: forall w v r . (KnownNat w, Member (Operations v) r, Member LogInstructionFetch r, Member (Decoding v) r, Member (ExprEval v) r) => v -> Eff r ()
buildAST entry =
    let
        !width = fromIntegral (intValue (knownNat :: NatRepr w))
    in writePC (FromImm entry) >> buildInstruction @v width
