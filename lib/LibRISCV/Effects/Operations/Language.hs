{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Implements an effect for interactions with the architectural state, upon
-- which instructions are executed (register file, memory, program counter, etc.).
module LibRISCV.Effects.Operations.Language where

import Control.Monad.Freer.TH (makeEffect)

-- | Abstraction for expressing a 8-, 16-, or 32-bit size.
data Size = Byte | Half | Word
    deriving (Eq, Show)

-- | Returns the size in bits (either 8, 16, or 32).
bitSize :: Size -> Int
bitSize Byte = 8
bitSize Half = 16
bitSize Word = 32

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
