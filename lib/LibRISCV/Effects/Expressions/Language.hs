{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Implements an effect for /evaluation/ of the 'Expr' abstraction.
module LibRISCV.Effects.Expressions.Language (
    ExprEval (..),
    eval,
    defaultEval,
    ifExprM,
    whenExprM,
    unlessExprM,
) where

import Control.Monad.Freer (Eff, Member, type (~>))
import Control.Monad.Freer.TH (makeEffect)
import Control.Monad.IO.Class (MonadIO)
import LibRISCV.Effects.Expressions.Expr (Expr)

-- TODO: Reevaluate if we really need the distinction between IsTrue / IsFalse
-- for symbolic execution or if this is just nice to have for debugging.
data ExprEval v r where
    IsTrue :: Expr v -> ExprEval v Bool
    IsFalse :: Expr v -> ExprEval v Bool
    Eval :: Expr v -> ExprEval v v

makeEffect ''ExprEval

defaultEval :: (MonadIO m) => (v -> Bool, Expr v -> v) -> ExprEval v ~> m
defaultEval (pred, evalE) = \case
    Eval e -> pure $ evalE e
    IsTrue e -> pure $ pred $ evalE e
    IsFalse e -> pure $ not . pred $ evalE e

------------------------------------------------------------------------

-- Higher-level abstraction for making use of IsTrue / IsFalse without
-- having to export the isTrue / isFalse smart constructor. The standard
-- constructor is still exported as it will need to be interpreted.

condExprM ::
    forall v r v'.
    (Member (ExprEval v) r) =>
    (Expr v -> Eff r Bool) ->
    Expr v ->
    Eff r v' ->
    Eff r v' ->
    Eff r v'
condExprM p b t f = do
    b <- p b
    if b then t else f

-- | Like 'Control.Monad.Extra.ifM' but with internal expression evaluation.
ifExprM :: forall v r v'. (Member (ExprEval v) r) => Expr v -> Eff r v' -> Eff r v' -> Eff r v'
ifExprM = (condExprM isTrue)

-- | Like 'Control.Monad.Extra.whenM' but with internal expression evaluation.
whenExprM :: forall v r. (Member (ExprEval v) r) => Expr v -> Eff r () -> Eff r ()
whenExprM b t = (condExprM isTrue) b t (pure ())

-- | Like 'Control.Monad.Extra.unlessM' but with internal expression evaluation.
unlessExprM :: forall v r. (Member (ExprEval v) r) => Expr v -> Eff r () -> Eff r ()
unlessExprM b t = (condExprM isFalse) b t (pure ())
