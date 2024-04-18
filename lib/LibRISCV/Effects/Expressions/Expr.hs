{-# LANGUAGE TemplateHaskell #-}

-- | Defines the expression abstraction to express arithmetic and logic
-- operations within the formal description of RISC-V instructions. The
-- abstraction is just a non-monadic algebraic data type called 'Expr'.
-- In addition to the the data type definition, this module also provides
-- several smart constructors for utilzing the expression lanuage, these
-- are generated using template-haskell.
module LibRISCV.Effects.Expressions.Expr (
  module LibRISCV.Effects.Expressions.Type,
  module LibRISCV.Effects.Expressions.Expr,
) where

import Data.Function (on)
import Data.BitVector (BV)
import LibRISCV.Effects.Expressions.Type
import LibRISCV.Effects.Expressions.Generator

-- | Extract shamt value from an expression (lower 5 bits).
regShamt :: Int -> Expr a -> Expr a
regShamt w a = a `And` FromInt w 0x1f

------------------------------------------------------------------------

zextImm :: Int -> a -> Expr a
zextImm w = ZExt w . FromImm

sextImm :: Int -> a -> Expr a
sextImm w = SExt w . FromImm

generateImmediates
