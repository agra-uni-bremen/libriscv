{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module LibRISCV.Machine.Interpreter where

import Data.Bits hiding (Xor, And)
import Data.Int
import Data.Word
import Data.Array.IO (IOUArray)
import LibRISCV
import LibRISCV.Spec.Expr
import Control.Monad.Freer
import Numeric (showHex)
import Data.Function (on)

import qualified LibRISCV.Machine.Register as REG
import qualified LibRISCV.Machine.Memory as MEM
import LibRISCV.Spec.Operations
import LibRISCV.Utils (boolToWord)
import Control.Monad.Freer.Reader (Reader, ask)
import LibRISCV.Decoder.Instruction (mkRd, mkRs1, mkRs2, immI, immS, immU, immB, immJ, mkShamt)
import Control.Monad (when, unless)
import Data.BitVector (BV (nat), zeroExtend, signExtend, bitVec)
import Data.Bool (bool)
import Debug.Trace

-- Architectural state of the executor.
type ArchState = (REG.RegisterFile IOUArray Register, MEM.Memory IOUArray Word8)

mkArchState :: Address -> Word32 -> IO ArchState
mkArchState memStart memSize = do
    reg <- REG.mkRegFile 0
    mem <- MEM.mkMemory memStart memSize
    pure (reg, mem)

dumpState :: ArchState -> IO ()
dumpState (r, _) =
    REG.dumpRegs (showHex . fromIntegral @Int32 @Word32) r >>= putStr

instance ByteAddrsMem ArchState where
    storeByteString (_, mem) = MEM.storeByteString mem

------------------------------------------------------------------------

runExpression :: Expr BV -> BV
runExpression (FromImm n a) = bitVec n a
runExpression (FromUInt n i) = bitVec n i
runExpression (ZExt n e) = zeroExtend n (runExpression e)
runExpression (SExt n e) = signExtend n (runExpression e)
runExpression (Add e1 e2) = runExpression e1 + runExpression e2
runExpression (Sub e1 e2) = fromIntegral $
    (fromIntegral (runExpression e1) :: Int32) - fromIntegral (runExpression e2)
runExpression (Eq e1 e2) = bool 0 1 $ (fromIntegral (runExpression e1) :: Int32) == fromIntegral (runExpression e2)
runExpression (Slt e1 e2) = bool 0 1 $ (fromIntegral (runExpression e1) :: Int32) < fromIntegral (runExpression e2)
runExpression (Sge e1 e2) = bool 0 1 $ (fromIntegral (runExpression e1) :: Int32) >= fromIntegral (runExpression e2)
runExpression (Ult e1 e2) = bool 0 1 $ runExpression e1 < runExpression e2
runExpression (Uge e1 e2) = bool 0 1 $ runExpression e1 >= runExpression e2
runExpression (And e1 e2) = runExpression e1 .&. runExpression e2
runExpression (Or e1 e2) = runExpression e1 .|. runExpression e2
runExpression (Xor e1 e2) = runExpression e1 `xor` runExpression e2
runExpression (LShl e1 e2) = runExpression e1 `unsafeShiftL` fromIntegral (fromIntegral (runExpression e2) :: Word8)
runExpression (LShr e1 e2) = runExpression e1 `unsafeShiftR` fromIntegral (fromIntegral (runExpression e2) :: Word8)
runExpression (AShr e1 e2) = fromIntegral $ (fromIntegral (runExpression e1) :: Int32) `unsafeShiftR` fromIntegral (fromIntegral (runExpression e2) :: Word8)
runExpression (Mul e1 e2) = runExpression e1 * runExpression e2
runExpression (SDiv e1 e2) = fromIntegral $ fromIntegral @_ @Int32 (runExpression e1) `quot` fromIntegral (runExpression e2)
runExpression (SRem e1 e2) = fromIntegral $ fromIntegral @_ @Int32 (runExpression e1) `rem` fromIntegral (runExpression e2)
runExpression (UDiv e1 e2) = runExpression e1 `quot` runExpression e2
runExpression (URem e1 e2) = runExpression e1 `rem` runExpression e2


type DefaultEnv = (Expr BV -> BV, ArchState)

runInstruction :: forall r effs env mem. (Member (Reader env) effs , LastMember IO effs) => 
    (env -> Operations mem ~> IO) -> Eff (Operations mem ': effs) r -> Eff effs r
runInstruction f eff =
    ask >>= \env -> interpretM (f env) eff

defaultBehavior :: DefaultEnv -> Operations BV ~> IO
defaultBehavior env@(evalE , (regFile, mem)) = \case
    DecodeRD inst -> pure (bitVec 32 $ mkRd $ fromIntegral $ nat inst)
    DecodeRS1 inst -> pure (bitVec 32 $ mkRs1 $ fromIntegral $ nat inst)
    DecodeRS2 inst -> pure (bitVec 32 $ mkRs2 $ fromIntegral $ nat inst)
    DecodeImmI inst -> pure (bitVec 32 $ immI $ fromIntegral $ nat inst)
    DecodeImmS inst -> pure (bitVec 32 $ immS $ fromIntegral $ nat inst)
    DecodeImmB inst -> pure (bitVec 32 $ immB $ fromIntegral $ nat inst)
    DecodeImmU inst -> pure (bitVec 32 $ immU $ fromIntegral $ nat inst)
    DecodeImmJ inst -> pure (bitVec 32 $ immJ $ fromIntegral $ nat inst)
    DecodeShamt inst -> pure (bitVec 32 $ mkShamt $ fromIntegral $ nat inst)
    RunIf e next -> when (evalE e == 1) $ defaultBehavior (evalE, (regFile, mem)) next
    RunIfElse e ifB elseB -> defaultBehavior (evalE, (regFile, mem)) $ if evalE e == 1 then ifB else elseB
    RunUnless e next -> unless (evalE e == 1) $ defaultBehavior (evalE, (regFile, mem)) next
    ReadRegister idx -> bitVec 32 <$> REG.readRegister regFile (toEnum $ fromIntegral (evalE $ FromImm 32 idx))
    WriteRegister idx reg -> REG.writeRegister regFile (toEnum $ fromIntegral idx) (fromIntegral $ evalE reg)
    LoadByte addr -> bitVec 32 <$> MEM.loadByte mem (fromIntegral $ evalE addr)
    LoadHalf addr -> bitVec 32 <$> (MEM.loadHalf mem (fromIntegral $ evalE addr) :: IO Word16)
    LoadWord addr -> bitVec 32 <$> MEM.loadWord @_ @_ @BV mem (fromIntegral $ evalE addr)
    StoreByte addr w -> MEM.storeByte mem (fromIntegral $ evalE addr) (fromIntegral $ evalE w)
    StoreHalf addr w -> MEM.storeHalf mem (fromIntegral $ evalE addr) (evalE w) 
    StoreWord addr w -> MEM.storeWord mem (fromIntegral $ evalE addr) (evalE w)
    WritePC w -> REG.writePC regFile (fromIntegral $ evalE w)
    ReadPC -> bitVec 32 <$> REG.readPC regFile
    Exception pc msg -> error $ "[0x" ++ showHex pc "" ++ "] " ++ msg
    Ecall pc -> putStrLn $ "ecall at 0x" ++ showHex pc ""
    Ebreak pc -> putStrLn $ "ebreak at 0x" ++ showHex pc ""
    Append__ s s' -> do
        defaultBehavior env s 
        defaultBehavior env s'
