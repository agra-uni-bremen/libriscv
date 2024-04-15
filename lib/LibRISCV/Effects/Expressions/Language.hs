{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}
module LibRISCV.Effects.Expressions.Language where

import LibRISCV.Effects.Expressions.Expr (Expr)
import Control.Monad.Freer.TH (makeEffect)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Freer ( type (~>) ) 


data ExprEval v r where
    IsTrue :: Expr v -> ExprEval v Bool
    IsFalse :: Expr v -> ExprEval v Bool
    Eval :: Expr v  -> ExprEval v v

makeEffect ''ExprEval

defaultEval :: (MonadIO m) => (v -> Bool, Expr v -> v) -> ExprEval v ~> m
defaultEval (pred, evalE) = \case
    Eval e  -> pure $ evalE e
    IsTrue e -> pure $ pred $ evalE e
    IsFalse e -> pure $ not . pred $ evalE e
