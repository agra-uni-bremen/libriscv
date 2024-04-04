{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module LibRISCV.Effects.Expressions.Default.Interpreter where
import Control.Monad.IO.Class ( MonadIO )
import LibRISCV.Effects.Expressions.Language ( ExprEval(..) )
import LibRISCV.Effects.Expressions.Expr ( Expr )
import Control.Monad.Freer ( type (~>) )

defaultEval :: (MonadIO m) => (v -> Bool, Expr v -> v) -> ExprEval v ~> m
defaultEval (pred, evalE) = pure . \case
    Eval e  -> evalE e
    IsTrue e -> pred $ evalE e
    IsFalse e -> not . pred $ evalE e
