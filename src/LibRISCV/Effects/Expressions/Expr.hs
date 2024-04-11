{-# LANGUAGE TemplateHaskell #-}

-- This module provides an additional abstraction for the LibRISCV
-- expression language. Some of the provided abstractions are generated
-- using template-haskell and LibRISCV.Effects.Expressions.Generator.
module LibRISCV.Effects.Expressions.Expr (
  module LibRISCV.Effects.Expressions.Type,
  module LibRISCV.Effects.Expressions.Expr,
) where

import Data.Function (on)
import Data.BitVector (BV)
import LibRISCV.Effects.Expressions.Type
import LibRISCV.Effects.Expressions.Generator

-- Extract shamt value from an expression (lower 5 bits).
regShamt :: Int -> Expr a -> Expr a
regShamt w a = a `And` FromInt w 0x1f

------------------------------------------------------------------------

genImmRvals
