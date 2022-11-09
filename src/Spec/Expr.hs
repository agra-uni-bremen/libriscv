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
    AddU (Expr a) (Expr a) |
    AddS (Expr a) (Expr a) |
    Slt  (Expr a) (Expr a) |
    SltU (Expr a) (Expr a) |
    And  (Expr a) (Expr a) |
    Or   (Expr a) (Expr a) |
    Xor  (Expr a) (Expr a) |
    LShl (Expr a) (Expr a)

addSImm :: a -> a -> Expr a
addSImm a b = (FromImm a) `AddS` (FromImm b)

addSInt :: a -> Int32 -> Expr a
addSInt a b = (FromImm a) `AddS` (FromInt b)

andInt :: a -> Int32 -> Expr a
andInt a b = (FromImm a) `And` (FromInt b)

orInt :: a -> Int32 -> Expr a
orInt a b = (FromImm a) `Or` (FromInt b)

xorInt :: a -> Int32 -> Expr a
xorInt a b = (FromImm a) `Xor` (FromInt b)

lshlInt :: a -> Word32 -> Expr a
lshlInt a b = (FromImm a) `LShl` (FromUInt b)
