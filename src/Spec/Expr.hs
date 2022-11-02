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
    BAnd (Expr a) (Expr a) |
    AddU (Expr a) (Expr a) |
    AddS (Expr a) (Expr a) |
    Slt (Expr a) (Expr a)

addSImm :: a -> a -> Expr a
addSImm a b = (FromImm a) `AddS` (FromImm b)

addSInt :: a -> Int32 -> Expr a
addSInt a b = (FromImm a) `AddS` (FromInt b)
