{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}

-- | Implements an effect for /evaluation/ of the 'Expr' abstraction. The effect
-- differentiates evaluating to a 'Bool' (via 'IsTrue' / 'IsFalse') and evaluatation
-- to the enclosing value type (via 'Eval').
module LibRISCV.Effects.Expressions.Language where

import LibRISCV.Effects.Expressions.Expr (Expr)
import Control.Monad.Freer.TH (makeEffect)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Freer ( type (~>) )

-- TODO: The distinction between IsTrue (i.e. take the path it evaluates to true)
-- and IsFalse is primarily interesting for symbolic execution as we need to know
-- if a branch is (not) taken depending on the expression. Would be cool to come
-- up with a better abstraction for this.
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
