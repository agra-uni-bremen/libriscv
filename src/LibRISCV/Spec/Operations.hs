{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
module LibRISCV.Spec.Operations where

import LibRISCV
import LibRISCV.Spec.Expr (Expr)
import Control.Monad.Freer.TH

data Operations v r where
    ReadRegister :: RegIdx -> Operations v v
    WriteRegister :: RegIdx -> Expr v -> Operations v ()
    LoadByte :: Expr v -> Operations v v
    LoadHalf :: Expr v -> Operations v v
    LoadWord :: Expr v -> Operations v v
    StoreByte :: Expr v -> Expr v -> Operations v ()
    StoreHalf :: Expr v -> Expr v -> Operations v ()
    StoreWord :: Expr v -> Expr v -> Operations v ()
    WritePC :: Expr v -> Operations v ()
    ReadPC :: Operations v v
    Ecall :: v -> Operations v ()
    Ebreak :: v -> Operations v ()
    LiftE :: Expr v -> Operations v v

makeEffect ''Operations