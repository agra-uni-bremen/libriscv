module Spec.Expr where

import Data.Bits
import Data.Int
import Data.Word
import Control.Monad (when)
import Control.Monad.Freer 
import Control.Monad.Freer.TH

import Common.Types
import Effects.Logging.InstructionFetch
import Data.Function ((&))
import Decoder (Immediate)
import Control.Monad.Freer.Error
import Control.Monad.Freer.State
import Conversion

data Expr a =
    FromImm a |
    FromInt Int32 |
    FromUInt Word32 |
    ZExtByte (Expr a) |
    ZExtHalf (Expr a) |
    SExtByte (Expr a) |
    SExtHalf (Expr a) |
    AddU (Expr a) (Expr a) |
    AddS (Expr a) (Expr a) |
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
addSImm a b = (FromImm a) `AddS` (FromImm b)

addSInt :: a -> Int32 -> Expr a
addSInt a b = (FromImm a) `AddS` (FromInt b)

andInt :: a -> Int32 -> Expr a
andInt a b = (FromImm a) `And` (FromInt b)

andImm :: a -> a -> Expr a
andImm a b = (FromImm a) `And` (FromImm b)

orInt :: a -> Int32 -> Expr a
orInt a b = (FromImm a) `Or` (FromInt b)

orImm :: a -> a -> Expr a
orImm a b = (FromImm a) `Or` (FromImm b)

xorInt :: a -> Int32 -> Expr a
xorInt a b = (FromImm a) `Xor` (FromInt b)

xorImm :: a -> a -> Expr a
xorImm a b = (FromImm a) `Xor` (FromImm b)

lshlInt :: a -> Word32 -> Expr a
lshlInt a b = (FromImm a) `LShl` (FromUInt b)

lshlImm :: a -> a -> Expr a
lshlImm a b = (FromImm a) `LShl` (FromImm b)

lshrInt :: a -> Word32 -> Expr a
lshrInt a b = (FromImm a) `LShr` (FromUInt b)

ashrInt :: a -> Word32 -> Expr a
ashrInt a b = (FromImm a) `AShr` (FromUInt b)

------------------------------------------------------------------------

-- Extract shamt value from an expression (lower 5 bits).
regShamt :: Expr a -> Expr a
regShamt a = a `And` (FromUInt 0x1f)
