{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

module LibRISCV.Effects.Operations.Default.Interpreter where

import Data.Int (Int32)
import Control.Monad.IO.Class ( MonadIO(..) )
import LibRISCV.Effects.Operations.Language
    ( Operations(..), Size(Word, Byte, Half) )
import Data.Array.IO (IOUArray)
import LibRISCV ( Address )
import qualified LibRISCV.Effects.Operations.Default.Machine.Memory as MEM
import qualified LibRISCV.Effects.Operations.Default.Machine.Register as REG
import Data.Word ( Word8, Word16, Word32 )
import Data.Int ( Int32 )
import Data.BitVector ( bitVec, BV )
import Control.Monad.Freer ( type (~>) )
import Numeric (showHex)


-- Architectural state of the executor.
type ArchState = (REG.RegisterFile IOUArray Int32, MEM.Memory IOUArray Word8)

mkArchState :: Address -> Word32 -> IO ArchState
mkArchState memStart memSize = do
    reg <- REG.mkRegFile 0
    mem <- MEM.mkMemory memStart memSize
    pure (reg, mem)

dumpState :: ArchState -> IO ()
dumpState (r, _) =
    REG.dumpRegs (showHex . fromIntegral @Int32 @Word32) r >>= putStr

type DefaultInstructionsEnv = ArchState

defaultInstructions :: MonadIO m => DefaultInstructionsEnv -> Operations BV ~> m
defaultInstructions (regFile, mem) = liftIO . \case
    ReadRegister idx -> bitVec 32 <$> REG.readRegister regFile (toEnum $ fromIntegral idx)
    WriteRegister idx reg -> REG.writeRegister regFile (toEnum $ fromIntegral idx) (fromIntegral reg)
    Load size addr -> case size of
        Byte -> bitVec 8 <$> MEM.loadByte mem (fromIntegral addr)
        Half -> bitVec 16 <$> (MEM.loadHalf mem (fromIntegral addr) :: IO Word16)
        Word -> bitVec 32 <$> MEM.loadWord @_ @_ @BV mem (fromIntegral addr)
    Store size addr w -> case size of
        Byte -> MEM.storeByte mem (fromIntegral addr) (fromIntegral w)
        Half -> MEM.storeHalf mem (fromIntegral addr) w
        Word -> MEM.storeWord mem (fromIntegral addr) w
    WritePC w -> REG.writePC regFile (fromIntegral w)
    ReadPC -> bitVec 32 <$> REG.readPC regFile
    Exception pc msg -> error $ "[0x" ++ showHex pc "" ++ "] " ++ msg
    Ecall pc -> putStrLn $ "ecall at 0x" ++ showHex pc ""
    Ebreak pc -> putStrLn $ "ebreak at 0x" ++ showHex pc ""
