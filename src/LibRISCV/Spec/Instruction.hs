{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module LibRISCV.Spec.Instruction where

import LibRISCV
import LibRISCV.Spec.Expr (Expr)
import Control.Monad.Freer.TH

data Instruction v r where
    ReadRegister :: RegIdx -> Instruction v v
    WriteRegister :: RegIdx -> Expr v -> Instruction v ()
    LoadByte :: Expr v -> Instruction v v
    LoadHalf :: Expr v -> Instruction v v
    LoadWord :: Expr v -> Instruction v v
    StoreByte :: Expr v -> Expr v -> Instruction v ()
    StoreHalf :: Expr v -> Expr v -> Instruction v ()
    StoreWord :: Expr v -> Expr v -> Instruction v ()
    WritePC :: Expr v -> Instruction v ()
    ReadPC :: Instruction v v
    Ecall :: v -> Instruction v ()
    Ebreak :: v -> Instruction v ()
    LiftE :: Expr v -> Instruction v v

makeEffect ''Instruction
