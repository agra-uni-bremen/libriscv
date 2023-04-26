{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
module Interpreter where

import Data.Int
import Data.Bits hiding (Xor, And)
import Data.Word
import Data.Array.IO (IOArray)

import LibRISCV
import LibRISCV.Spec.Expr
import LibRISCV.Spec.Operations
import LibRISCV.Decoder.Instruction (mkRd, mkRs1, mkRs2, immI, immS, immU, immB, immJ, mkShamt)
import Control.Monad.Freer
import Conversion
import Numeric (showHex)
import Data.BitVector (BV, bitVec)

import qualified LibRISCV.Machine.Interpreter as STD
import qualified LibRISCV.Machine.Register as REG
import qualified LibRISCV.Machine.Memory as MEM

data Tainted a = MkTainted Bool a

instance Functor Tainted where
    fmap proc (MkTainted t v) = MkTainted t $ proc v

instance Conversion a (Tainted a) where
    convert = MkTainted False
instance Conversion (Tainted a) a where
    convert (MkTainted _ v) = v

------------------------------------------------------------------------
{-
fromBytes :: Num b => [Tainted Word8] -> Tainted b
fromBytes v = MkTainted tainted (fromIntegral $ MEM.mkWord bytes)
    where
        tainted = any (\(MkTainted t _) -> t) v
        bytes   = map (\(MkTainted _ v) -> v) v

fromInt :: Integral b => Tainted b -> [Tainted Word8]
fromInt (MkTainted t v) = map (\x -> MkTainted t x :: Tainted Word8) (MEM.mkBytes $ fromIntegral v)

instance MEM.WordStorage (Tainted Word32) (Tainted Word8) where
    toWord = fromBytes
    wordToBytes = fromInt

instance MEM.HalfStorage (Tainted Word16) (Tainted Word8) where
    toHalf = fromBytes
    halfToBytes = fromInt

------------------------------------------------------------------------

-- Architectural state of the executor.
type ArchState = (REG.RegisterFile IOArray (Tainted Word32), MEM.Memory IOArray (Tainted Word8))

mkArchState :: Address -> Word32 -> IO ArchState
mkArchState memStart memSize = do
    reg <- REG.mkRegFile $ MkTainted False (bitVec 32 0)
    mem <- MEM.mkMemory memStart memSize
    pure (reg, mem)

dumpState :: ArchState -> IO ()
dumpState (r, m) = REG.dumpRegs showTainted r >>= putStr
    where
        showTainted (MkTainted t v) = (++) $ (showHex v) (if t then " (tainted)" else "")

instance ByteAddrsMem ArchState where
    storeByteString (_, mem) = MEM.storeByteString mem

------------------------------------------------------------------------

-- Execute a binary operation with tainted values.
binOp :: Expr (Tainted BV) -> Expr (Tainted BV) -> (Expr BV -> Expr BV -> Expr BV) -> Tainted BV
binOp e1 e2 op = MkTainted (t1 || t2) $
        STD.runExpression (FromImm 32 v1 `op` FromImm 32 v2)
    where
        (MkTainted t1 v1) = runExpression e1
        (MkTainted t2 v2) = runExpression e2

-- Execute a unary operation with tainted values.
unOp :: Expr (Tainted BV) -> (Expr BV -> Expr BV) -> Tainted BV
unOp e1 op = MkTainted taint $ STD.runExpression (op $ FromImm 32 value)
    where
        (MkTainted taint value) = runExpression e1

runExpression :: Expr (Tainted BV) -> Tainted BV
runExpression (FromImm n t)  = t
runExpression (FromUInt n i) = MkTainted False i
runExpression (ZExt n e) = unOp e (ZExt n)
runExpression (SExt n e) = unOp e (SExt n)
runExpression (Add e1 e2)  = binOp e1 e2 Add
runExpression (Sub e1 e2)  = binOp e1 e2 Sub
runExpression (Eq e1 e2)   = binOp e1 e2 Eq
runExpression (Slt e1 e2)  = binOp e1 e2 Slt
runExpression (Sge e1 e2)  = binOp e1 e2 Sge
runExpression (Ult e1 e2)  = binOp e1 e2 Ult
runExpression (Uge e1 e2)  = binOp e1 e2 Uge
runExpression (And e1 e2)  = binOp e1 e2 And
runExpression (Or e1 e2)   = binOp e1 e2 Or
runExpression (Xor e1 e2)  = binOp e1 e2 Xor
runExpression (LShl e1 e2) = binOp e1 e2 LShl
runExpression (LShr e1 e2) = binOp e1 e2 LShr
runExpression (AShr e1 e2) = binOp e1 e2 AShr
runExpression (Mul e1 e2) = binOp e1 e2 Mul
runExpression (SDiv e1 e2) = binOp e1 e2 SDiv
runExpression (UDiv e1 e2) = binOp e1 e2 UDiv
runExpression (SRem e1 e2) = binOp e1 e2 SRem
runExpression (URem e1 e2) = binOp e1 e2 URem

------------------------------------------------------------------------

type IftEnv = (Expr (Tainted BV) -> Tainted BV, ArchState)

iftBehavior :: IftEnv -> Operations (Tainted Word32) ~> IO
iftBehavior env@(evalE , (regFile, mem)) = \case
    DecodeRD inst -> pure $ convert (mkRd $ convert inst)
    DecodeRS1 inst -> pure $ convert (mkRs1 $ convert inst)
    DecodeRS2 inst -> pure $ convert (mkRs2 $ convert inst)
    DecodeImmI inst -> pure $ convert (immI $ convert inst)
    DecodeImmS inst -> pure $ convert (immS $ convert inst)
    DecodeImmB inst -> pure $ convert (immB $ convert inst)
    DecodeImmU inst -> pure $ convert (immU $ convert inst)
    DecodeImmJ inst -> pure $ convert (immJ $ convert inst)
    DecodeShamt inst -> pure $ convert (mkShamt $ convert inst)

    RunIf e next -> undefined
    RunIfElse e ifB elseB -> undefined
    RunUnless e next -> undefined
    ReadRegister idx -> undefined
    WriteRegister idx reg -> undefined
    LoadByte addr -> undefined
    LoadHalf addr -> undefined
    LoadWord addr -> undefined
    StoreByte addr w -> undefined
    StoreHalf addr w -> undefined
    StoreWord addr w -> undefined
    WritePC w -> undefined
    ReadPC -> undefined
    Exception _ msg -> error msg
    Ecall _ -> putStrLn "ECALL"
    Ebreak _ -> putStrLn "EBREAK"

    Append__ s s' -> do
        iftBehavior env s
        iftBehavior env s'
