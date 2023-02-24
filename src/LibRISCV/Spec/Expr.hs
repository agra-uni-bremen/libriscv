module LibRISCV.Spec.Expr where

import Data.Word

data Expr a =
    FromImm a |
    FromUInt Word32 |
    ZExtByte (Expr a) |
    ZExtHalf (Expr a) |
    SExtByte (Expr a) |
    SExtHalf (Expr a) |
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
    AShr (Expr a) (Expr a)

addSImm :: a -> a -> Expr a
addSImm a b = FromImm a `Add` FromImm b

andImm :: a -> a -> Expr a
andImm a b = FromImm a `And` FromImm b

orImm :: a -> a -> Expr a
orImm a b = FromImm a `Or` FromImm b

xorImm :: a -> a -> Expr a
xorImm a b = FromImm a `Xor` FromImm b

lshlImm :: a -> a -> Expr a
lshlImm a b = FromImm a `LShl` FromImm b

lshrImm :: a -> a -> Expr a
lshrImm a b = FromImm a `LShr` FromImm b

ashrImm :: a -> a -> Expr a
ashrImm a b = FromImm a `AShr` FromImm b

------------------------------------------------------------------------

-- Extract shamt value from an expression (lower 5 bits).
regShamt :: Expr a -> Expr a
regShamt a = a `And` FromUInt 0x1f
