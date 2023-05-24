{-# LANGUAGE TypeApplications #-}
module LibRISCV.Effects.Expressions.Default.EvalE where

import LibRISCV.Effects.Expressions.Expr ( Expr(..) )
import Data.BitVector (BV, Bits (unsafeShiftR, unsafeShiftL, xor, (.|.), (.&.)), signExtend, zeroExtend, bitVec, ones)
import Data.Int ( Int32 )
import Data.Word ( Word8 )
import Data.Bool ( bool )

evalE :: Expr BV -> BV
evalE (FromImm n a) = bitVec n a
evalE (FromUInt n i) = bitVec n i
evalE (ZExt n e) = zeroExtend n (evalE e)
evalE (SExt n e) = signExtend n (evalE e)
evalE (Extract start len e) = evalE e `unsafeShiftR` start .&. ones len
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