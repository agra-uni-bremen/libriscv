module LibRISCV.Effects.Expressions.Expr where

import Data.Word
import Data.Function (on)
import Data.Int (Int32)
import Data.BitVector (BV)

data Expr a =
    FromImm a |
    FromInt Int Integer |
    ZExt Int (Expr a) |
    SExt Int (Expr a) |
    Extract Int Int (Expr a) |
    Add  (Expr a) (Expr a) |
    Sub  (Expr a) (Expr a) |
    Eq   (Expr a) (Expr a) |
    Slt  (Expr a) (Expr a) |
    Sge  (Expr a) (Expr a) |
    Ult  (Expr a) (Expr a) |
    Uge  (Expr a) (Expr a) |
    And  (Expr a) (Expr a) |
    Or   (Expr a) (Expr a) |
    Xor  (Expr a) (Expr a) |
    LShl (Expr a) (Expr a) |
    LShr (Expr a) (Expr a) |
    AShr (Expr a) (Expr a) |
    Mul (Expr a) (Expr a) |
    UDiv (Expr a) (Expr a) |
    SDiv (Expr a) (Expr a) |
    URem (Expr a) (Expr a) |
    SRem (Expr a) (Expr a) 

addSImm :: a -> a -> Expr a
addSImm = Add `on` FromImm

andImm :: a -> a -> Expr a
andImm = And `on` FromImm

orImm :: a -> a -> Expr a
orImm = Or `on` FromImm

xorImm :: a -> a -> Expr a
xorImm =  Xor `on` FromImm

lshlImm :: a -> a -> Expr a
lshlImm = LShl `on` FromImm

lshrImm :: a -> a -> Expr a
lshrImm = LShr `on` FromImm

ashrImm :: a -> a -> Expr a
ashrImm = AShr `on` FromImm


------------------------------------------------------------------------

-- Extract shamt value from an expression (lower 5 bits).
regShamt :: Int -> Expr a -> Expr a
regShamt w a = a `And` FromInt w 0x1f
