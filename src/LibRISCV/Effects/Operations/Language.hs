{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
module LibRISCV.Effects.Operations.Language where

import LibRISCV.Effects.Expressions.Expr (Expr)
import Control.Monad.Freer.TH ( makeEffect )
import LibRISCV.Decoder.Opcodes (InstructionType)

data Size = Byte | Half | Word deriving (Eq, Show)

data Operations v r where
    ReadRegister :: v -> Operations v v
    WriteRegister :: v -> v -> Operations v ()
    Load :: Size -> v -> Operations v v
    Store :: Size -> v -> v -> Operations v ()
    WritePC :: v -> Operations v ()
    ReadPC :: Operations v v
    Exception :: v -> String -> Operations v ()
    Ecall :: v -> Operations v ()
    Ebreak :: v -> Operations v ()

makeEffect ''Operations