{-# LANGUAGE TypeSynonymInstances #-}
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
import Data.Bits
import Data.Word
import Data.Array.IO (IOArray)
import Common.Types
import Spec.Expr
import Spec.Instruction
import Control.Monad.Freer
import Conversion
import Numeric (showHex)

import qualified Machine.Standard.Register as REG
import qualified Machine.Standard.Memory as MEM

data Tainted a = MkTainted Bool a

instance Functor Tainted where
    fmap proc (MkTainted t v) = MkTainted t $ proc v

instance Conversion a (Tainted a) where
    convert = MkTainted False
instance Conversion (Tainted a) a where
    convert (MkTainted _ v) = v

------------------------------------------------------------------------

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
    reg <- REG.mkRegFile $ MkTainted False (0 :: Word32)
    mem <- MEM.mkMemory memStart memSize
    pure (reg, mem)

dumpState :: ArchState -> IO ()
dumpState (r, m) = REG.dumpRegs showTainted r >>= putStr
    where
        showTainted (MkTainted t v) = (++) $ (showHex v) (if t then " (tainted)" else "")

instance ByteAddrsMem ArchState where
    storeByteString (_, mem) = MEM.storeByteString mem

------------------------------------------------------------------------

boolToWord :: Bool -> Word32
boolToWord True  = 1
boolToWord False = 0

runExpression :: Expr (Tainted Word32) -> (Tainted Word32)
runExpression (FromImm t) = t
runExpression (FromInt i) = MkTainted False $ fromIntegral i
runExpression (FromUInt i) = MkTainted False i
runExpression (ZExtByte a) = runExpression a
runExpression (ZExtHalf a) = runExpression a
runExpression (SExtByte e) = MkTainted t1 $ fromIntegral @Int8 @Word32 $ fromIntegral @Word8 @Int8 (fromIntegral @Word32 @Word8 v1)
    where
        (MkTainted t1 v1) = runExpression e
runExpression (SExtHalf e) = MkTainted t1 $ fromIntegral $ fromIntegral @Word16 @Int16 (fromIntegral @Word32 @Word16 v1)
    where
        (MkTainted t1 v1) = runExpression e
runExpression (AddU e1 e2) =
        MkTainted (t1 || t2) (v1 + v2)
    where
        (MkTainted t1 v1) = runExpression e1
        (MkTainted t2 v2) = runExpression e2
runExpression (AddS e1 e2) =
        MkTainted (t1 || t2) $ fromIntegral ((fromIntegral v1 :: Int32) + (fromIntegral v2))
    where
        (MkTainted t1 v1) = runExpression e1
        (MkTainted t2 v2) = runExpression e2
runExpression (Sub e1 e2) = MkTainted (t1 || t2) $ fromIntegral $
        (fromIntegral v1 :: Int32) - fromIntegral v2
    where
        (MkTainted t1 v1) = runExpression e1
        (MkTainted t2 v2) = runExpression e2
runExpression (Eq e1 e2) = MkTainted (t1 || t2) $ boolToWord $
        (fromIntegral v1 :: Int32) == fromIntegral v2
    where
        (MkTainted t1 v1) = runExpression e1
        (MkTainted t2 v2) = runExpression e2
runExpression (Slt e1 e2) = MkTainted (t1 || t2) $ boolToWord $
        (fromIntegral v1 :: Int32) < (fromIntegral v2)
    where
        (MkTainted t1 v1) = runExpression e1
        (MkTainted t2 v2) = runExpression e2
runExpression (Sge e1 e2) = MkTainted (t1 || t2) $ boolToWord $
        (fromIntegral v1 :: Int32) >= (fromIntegral v2)
    where
        (MkTainted t1 v1) = runExpression e1
        (MkTainted t2 v2) = runExpression e2
runExpression (Ult e1 e2) = MkTainted (t1 || t2) $ boolToWord $ v1 < v2
    where
        (MkTainted t1 v1) = runExpression e1
        (MkTainted t2 v2) = runExpression e2
runExpression (Uge e1 e2) = MkTainted (t1 || t2) $ boolToWord $ v1 >= v2
    where
        (MkTainted t1 v1) = runExpression e1
        (MkTainted t2 v2) = runExpression e2
runExpression (And e1 e2) =
        MkTainted (t1 || t2) (v1 .&. v2)
    where
        (MkTainted t1 v1) = runExpression e1
        (MkTainted t2 v2) = runExpression e2
runExpression (Or e1 e2) =
        MkTainted (t1 || t2) (v1 .|. v2)
    where
        (MkTainted t1 v1) = runExpression e1
        (MkTainted t2 v2) = runExpression e2
runExpression (Xor e1 e2) =
        MkTainted (t1 || t2) (v1 `xor` v2)
    where
        (MkTainted t1 v1) = runExpression e1
        (MkTainted t2 v2) = runExpression e2
runExpression (LShl e1 e2) = MkTainted (t1 || t2) $ v1 `unsafeShiftL` fromIntegral (fromIntegral v1 :: Word8)
    where
        (MkTainted t1 v1) = runExpression e1
        (MkTainted t2 v2) = runExpression e2
runExpression (LShr e1 e2) = MkTainted (t1 || t2) $ v1 `unsafeShiftR` fromIntegral (fromIntegral v2 :: Word8)
    where
        (MkTainted t1 v1) = runExpression e1
        (MkTainted t2 v2) = runExpression e2
runExpression (AShr e1 e2) = MkTainted (t1 || t2) $ fromIntegral $ (fromIntegral v1 :: Int32) `unsafeShiftR` fromIntegral (fromIntegral v2 :: Word8)
    where
        (MkTainted t1 v1) = runExpression e1
        (MkTainted t2 v2) = runExpression e2

type IftEnv = (Expr (Tainted Word32) -> (Tainted Word32), ArchState)

iftBehavior :: IftEnv -> Instruction (Tainted Word32) ~> IO
iftBehavior (evalE , (regFile, mem)) = \case
    (ReadRegister idx) -> REG.readRegister regFile idx
    (WriteRegister idx reg) -> REG.writeRegister regFile idx (evalE reg)
    (LoadByte addr) -> fmap fromIntegral <$> MEM.loadByte mem (convert $ evalE addr)
    (LoadHalf addr) -> fmap fromIntegral <$> (MEM.loadHalf mem (convert $ evalE addr) :: IO (Tainted Word16))
    (LoadWord addr) -> MEM.loadWord mem (convert $ evalE addr)
    (StoreByte addr w) -> MEM.storeByte mem (convert $ evalE addr) (fmap fromIntegral $ evalE w)
    (StoreHalf addr w) -> MEM.storeHalf mem (convert $ evalE addr) (fmap (fromIntegral @Word32 @Word16) $ evalE w)
    (StoreWord addr w) -> MEM.storeWord mem (convert $ evalE addr) (evalE w)
    (WritePC w) -> REG.writePC regFile (convert $ evalE w) -- TODO
    ReadPC -> MkTainted False <$> REG.readPC regFile       -- TODO
    (Ecall _) -> putStrLn "ECALL"
    (Ebreak _) -> putStrLn "EBREAK"
    LiftE e -> pure $ evalE e
