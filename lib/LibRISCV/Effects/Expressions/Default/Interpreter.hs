{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

-- | Provides the default (concrete) evaluation of the expression abstraction.
module LibRISCV.Effects.Expressions.Default.Interpreter where

import Data.Int ( Int32 )
import Data.Word ( Word8 )
import Data.Bool ( bool )
import Data.BitVector (BV, extract, Bits (unsafeShiftR, unsafeShiftL, xor, (.|.), (.&.)), signExtend, zeroExtend, bitVec, ones)
import Control.Monad.IO.Class ( MonadIO )
import LibRISCV.Effects.Expressions.Language ( ExprEval(..) )
import LibRISCV.Effects.Expressions.Expr ( Expr(..) )
import Control.Monad.Freer ( type (~>) )

-- | Evaluate an 'Expr' abstraction which encapsulates a concrete 'BV'.
evalE :: Expr BV -> BV
evalE (FromImm a) = a
evalE (FromInt n i) = bitVec n i
evalE (ZExt n e) = zeroExtend n (evalE e)
evalE (SExt n e) = signExtend n (evalE e)
evalE (Extract start len e) = extract (start + (len - 1)) start (evalE e)
evalE (Add e1 e2) = evalE e1 + evalE e2
evalE (Sub e1 e2) = fromIntegral $
    (fromIntegral (evalE e1) :: Int32) - fromIntegral (evalE e2)
evalE (Eq e1 e2) = bool 0 1 $ (fromIntegral (evalE e1) :: Int32) == fromIntegral (evalE e2)
evalE (Slt e1 e2) = bool 0 1 $ (fromIntegral (evalE e1) :: Int32) < fromIntegral (evalE e2)
evalE (Sge e1 e2) = bool 0 1 $ (fromIntegral (evalE e1) :: Int32) >= fromIntegral (evalE e2)
evalE (Ult e1 e2) = bool 0 1 $ evalE e1 < evalE e2
evalE (Uge e1 e2) = bool 0 1 $ evalE e1 >= evalE e2
evalE (And e1 e2) = evalE e1 .&. evalE e2
evalE (Or e1 e2) = evalE e1 .|. evalE e2
evalE (Xor e1 e2) = evalE e1 `xor` evalE e2
evalE (LShl e1 e2) = evalE e1 `unsafeShiftL` fromIntegral (fromIntegral (evalE e2) :: Word8)
evalE (LShr e1 e2) = evalE e1 `unsafeShiftR` fromIntegral (fromIntegral (evalE e2) :: Word8)
evalE (AShr e1 e2) = fromIntegral $ (fromIntegral (evalE e1) :: Int32) `unsafeShiftR` fromIntegral (fromIntegral (evalE e2) :: Word8)
evalE (Mul e1 e2) = evalE e1 * evalE e2
evalE (SDiv e1 e2) = fromIntegral $ fromIntegral @_ @Int32 (evalE e1) `quot` fromIntegral (evalE e2)
evalE (SRem e1 e2) = fromIntegral $ fromIntegral @_ @Int32 (evalE e1) `rem` fromIntegral (evalE e2)
evalE (UDiv e1 e2) = evalE e1 `quot` evalE e2
evalE (URem e1 e2) = evalE e1 `rem` evalE e2

-- | Concrete implementation of the 'ExprEval' effect.
defaultEval :: (MonadIO m) => (v -> Bool, Expr v -> v) -> ExprEval v ~> m
defaultEval (pred, evalE) = pure . \case
    Eval e  -> evalE e
    IsTrue e -> pred $ evalE e
    IsFalse e -> not . pred $ evalE e
