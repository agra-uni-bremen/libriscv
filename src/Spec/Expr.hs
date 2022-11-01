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

data Expr a where
    SExt :: (Conversion a Unsigned32) => a -> Expr Signed32
    Signed :: Signed32 -> Expr Signed32
    Unsigned :: Unsigned32 -> Expr Unsigned32
    LossyConvert :: (Integral a, Num b) => Expr a -> Expr b
    (:+:) :: (Num a, Conversion b (Expr a)) => Expr a -> b -> Expr a
    (:&:) :: (Bits a, Conversion b (Expr a)) => Expr a -> b -> Expr a
    (:<:) :: Ord a => Expr a -> Expr a -> Expr Bool

infixl 6 :+:
infixl 7 :&:
infix 4 :<:

instance Conversion Signed32 (Expr Signed32) where
    convert = Signed

instance Conversion Unsigned32 (Expr Unsigned32) where
    convert = Unsigned

instance Conversion (Expr Signed32) Signed32 where
    convert (Signed v) = v

instance Conversion (Expr Unsigned32) Unsigned32 where
    convert (Unsigned v) = v
