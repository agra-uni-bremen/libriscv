{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
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
