{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
module Machine.Standard.Interpreter where

import Conversion
import Data.Bits
import Data.Int
import Data.Word
import Common.Types
import Spec.Expr
import Spec.AST
import Control.Monad
import Control.Monad.Freer
import Control.Monad.Freer.TH

import qualified Machine.Standard.Register as REG
import qualified Machine.Standard.Memory as MEM

-- Architectural state of the executor.
type ArchState = (REG.RegisterFile Register, MEM.Memory Word8)

mkArchState :: Address -> Word32 -> IO ArchState
mkArchState memStart memSize = do
    reg <- REG.mkRegFile 0
    mem <- MEM.mkMemory memStart memSize
    pure (reg, mem)

dumpState :: ArchState -> IO ()
dumpState (r, m) =
    REG.dumpRegs r >>= putStr

instance ByteAddrsMem ArchState where
    storeByteString (_, mem) = MEM.storeByteString mem

------------------------------------------------------------------------

runExpression :: Expr Word32 -> Word32
runExpression (FromImm a) = a
runExpression (FromInt i) = fromIntegral i
runExpression (FromUInt i) = i
runExpression (BAnd e1 e2) = (runExpression e1) .&. (runExpression e2)
runExpression (AddU e1 e2) = (runExpression e1) + (runExpression e2)
runExpression (AddS e1 e2) = fromIntegral $
    (fromIntegral (runExpression e1) :: Int32) + (fromIntegral (runExpression e2))
runExpression (Slt e1 e2) = if
    (fromIntegral (runExpression e1) :: Int32) < (fromIntegral (runExpression e2))
        then 1
        else 0

runInstructionM :: forall r effs . LastMember IO effs => (Expr Word32 -> Word32) -> ArchState -> Eff (Instruction Word32 ': effs) r -> Eff effs r
runInstructionM evalE (regFile, mem) = interpretM $ \case
    (ReadRegister idx) -> fromIntegral <$> REG.readRegister regFile idx
    (WriteRegister idx reg) -> REG.writeRegister regFile idx (fromIntegral $ evalE reg)
    (LoadWord addr) -> MEM.loadWord mem (evalE addr)
    (StoreWord addr w) -> MEM.storeWord mem (evalE addr) (evalE w)
    (WritePC w) -> REG.writePC regFile (evalE w)
    ReadPC -> REG.readPC regFile
    LiftE e -> pure $ evalE e
