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
type ArchState = (REG.RegisterFile, MEM.Memory)

mkArchState :: Address -> Word32 -> IO ArchState
mkArchState memStart memSize = do
    reg <- REG.mkRegFile
    mem <- MEM.mkMemory memStart memSize
    pure (reg, mem)

dumpState :: ArchState -> IO ()
dumpState (r, m) =
    REG.dumpRegs r >>= putStr

instance ByteAddrsMem ArchState where
    storeByteString (_, mem) = MEM.storeByteString mem

------------------------------------------------------------------------

runExpression :: Expr a -> a
runExpression (Signed r) = r
runExpression (Unsigned a) = a
runExpression (LossyConvert e) = fromIntegral $ runExpression e
runExpression (e :+: e') = runExpression e + runExpression (convert e')
runExpression (e :&: e') = runExpression e .&. runExpression (convert e')
runExpression (e :<: e') = runExpression e < runExpression e'

runInstructionM :: forall r effs . LastMember IO effs => (forall a . Expr a -> a) -> ArchState -> Eff (Instruction ': effs) r -> Eff effs r 
runInstructionM evalE (regFile, mem) = interpretM $ \case
    (ReadRegister idx) -> Signed <$> REG.readRegister regFile idx
    (WriteRegister idx reg) -> REG.writeRegister regFile idx (evalE $ convert reg)
    (LoadWord addr) -> Unsigned <$> MEM.loadWord mem (evalE addr)
    (StoreWord addr w) -> MEM.storeWord mem (evalE addr) (evalE w)
    (WritePC w) -> REG.writePC regFile (evalE w)
    ReadPC -> Unsigned <$> REG.readPC regFile
    LiftE e -> pure $ evalE e
    UnexpectedError -> fail "Unexpected error"
