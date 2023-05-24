module LibRISCV.Effects.Expressions.Expr where

import Data.Word
import Data.Function (on)
import Data.Int (Int32)
import Data.BitVector (BV)

data Expr a =
    FromImm Int a |
    FromUInt Int BV |
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

fromImm32 :: a -> Expr a
fromImm32 = FromImm 32

fromImm64 :: a -> Expr a
fromImm64 = FromImm 64

fromImm128 :: a -> Expr a
fromImm128 = FromImm 128

addSImm :: a -> a -> Expr a
addSImm = Add `on` FromImm 32

andImm :: a -> a -> Expr a
andImm = And `on` FromImm 32

orImm :: a -> a -> Expr a
orImm = Or `on` FromImm 32

xorImm :: a -> a -> Expr a
xorImm =  Xor `on` FromImm 32

lshlImm :: a -> a -> Expr a
lshlImm = LShl `on` FromImm 32

lshrImm :: a -> a -> Expr a
lshrImm = LShr `on` FromImm 32

ashrImm :: a -> a -> Expr a
ashrImm = AShr `on` FromImm 32


------------------------------------------------------------------------

-- Extract shamt value from an expression (lower 5 bits).
regShamt :: Int -> Expr a -> Expr a
regShamt w a = a `And` FromUInt w 0x1f
