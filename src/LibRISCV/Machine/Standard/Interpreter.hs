{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module LibRISCV.Machine.Standard.Interpreter where

import Data.Bits hiding (Xor, And)
import Data.Int
import Data.Word
import Data.Array.IO (IOUArray)
import LibRISCV
import LibRISCV.Spec.Expr
import Control.Monad.Freer
import Numeric (showHex)

import qualified LibRISCV.Machine.Standard.Register as REG
import qualified LibRISCV.Machine.Standard.Memory as MEM
import LibRISCV.Spec.Instruction
import LibRISCV.Utils (boolToWord)
import Control.Monad.Freer.Reader (Reader, ask)

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

runExpression :: Expr Word32 -> Word32
runExpression (FromImm a) = a
runExpression (FromInt i) = fromIntegral i
runExpression (FromUInt i) = i
runExpression (ZExtByte a) = runExpression a
runExpression (ZExtHalf a) = runExpression a
runExpression (SExtByte e) = fromIntegral $ fromIntegral @Word8 @Int8 (fromIntegral @Word32 @Word8 (runExpression e))
runExpression (SExtHalf e) = fromIntegral $ fromIntegral @Word16 @Int16 (fromIntegral @Word32 @Word16 (runExpression e))
runExpression (AddU e1 e2) = runExpression e1 + runExpression e2
runExpression (AddS e1 e2) = fromIntegral $
    (fromIntegral (runExpression e1) :: Int32) + fromIntegral (runExpression e2)
runExpression (Sub e1 e2) = fromIntegral $
    (fromIntegral (runExpression e1) :: Int32) - fromIntegral (runExpression e2)
runExpression (Eq e1 e2) = boolToWord $
    (fromIntegral (runExpression e1) :: Int32) == fromIntegral (runExpression e2)
runExpression (Slt e1 e2) = boolToWord $
    (fromIntegral (runExpression e1) :: Int32) < fromIntegral (runExpression e2)
runExpression (Sge e1 e2) = boolToWord $
    (fromIntegral (runExpression e1) :: Int32) >= fromIntegral (runExpression e2)
runExpression (Ult e1 e2) = boolToWord $ runExpression e1 < runExpression e2
runExpression (Uge e1 e2) = boolToWord $ runExpression e1 >= runExpression e2
runExpression (And e1 e2) = runExpression e1 .&. runExpression e2
runExpression (Or e1 e2) = runExpression e1 .|. runExpression e2
runExpression (Xor e1 e2) = runExpression e1 `xor` runExpression e2
runExpression (LShl e1 e2) = runExpression e1 `unsafeShiftL` fromIntegral (fromIntegral (runExpression e2) :: Word8)
runExpression (LShr e1 e2) = runExpression e1 `unsafeShiftR` fromIntegral (fromIntegral (runExpression e2) :: Word8)
runExpression (AShr e1 e2) = fromIntegral $ (fromIntegral (runExpression e1) :: Int32) `unsafeShiftR` fromIntegral (fromIntegral (runExpression e2) :: Word8)

type DefaultEnv = (Expr Word32 -> Word32, ArchState)

runInstruction :: forall r effs env mem. (Member (Reader env) effs , LastMember IO effs) => 
    (env -> Instruction mem ~> IO) -> Eff (Instruction mem ': effs) r -> Eff effs r
runInstruction f eff =
    ask >>= \env -> interpretM (f env) eff

defaultBehavior :: DefaultEnv -> Instruction Word32 ~> IO
defaultBehavior (evalE , (regFile, mem)) = \case
    (ReadRegister idx) -> fromIntegral <$> REG.readRegister regFile idx
    (WriteRegister idx reg) -> REG.writeRegister regFile idx (fromIntegral $ evalE reg)
    (LoadByte addr) -> fromIntegral <$> MEM.loadByte mem (evalE addr)
    (LoadHalf addr) -> fromIntegral <$> (MEM.loadHalf mem (evalE addr) :: IO Word16)
    (LoadWord addr) -> MEM.loadWord mem (evalE addr)
    (StoreByte addr w) -> MEM.storeByte mem (evalE addr) (fromIntegral $ evalE w)
    (StoreHalf addr w) -> MEM.storeHalf mem (evalE addr) (fromIntegral (evalE w) :: Word16)
    (StoreWord addr w) -> MEM.storeWord mem (evalE addr) (evalE w)
    (WritePC w) -> REG.writePC regFile (evalE w)
    ReadPC -> REG.readPC regFile
    (Ecall pc) -> putStrLn $ "ecall at 0x" ++ showHex pc ""
    (Ebreak pc) -> putStrLn $ "ebreak at 0x" ++ showHex pc ""
    LiftE e -> pure $ evalE e
