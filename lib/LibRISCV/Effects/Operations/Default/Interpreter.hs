{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}

-- | Implements the default (concrete) interpreter for the 'Operations' effect.
module LibRISCV.Effects.Operations.Default.Interpreter
  ( ArchState(..)
  , mkArchState
  , dumpState
  , defaultInstructions
  )
where

import Data.Int ( Int32, Int32 )
import Control.Monad.IO.Class ( MonadIO(..) )
import LibRISCV.Effects.Operations.Language
    ( Operations(..), Size(Word, Byte, Half) )
import Data.Array.IO (IOUArray)
import LibRISCV ( Address )
import qualified LibRISCV.Effects.Operations.Default.Machine.Memory as MEM
import qualified LibRISCV.Effects.Operations.Default.Machine.Register as REG
import Data.Word ( Word8, Word16, Word32 )
import Data.BitVector ( bitVec, BV )
import Control.Monad.Freer ( type (~>) )
import Numeric (showHex)

-- | Representation of the concrete architectural state of the interpreter.
data ArchState = ArchState
  { getReg :: REG.RegisterFile IOUArray Int32
  -- | ^ Register file implementation of the architectural state.
  , getMem :: MEM.Memory IOUArray Word8
  -- | ^ Memory implementation of the architectural state.
  }

-- | Create a new t'ArchState' based on a memory start address and a memory size.
mkArchState :: Address -> Word32 -> IO ArchState
mkArchState memStart memSize = do
    reg <- REG.mkRegFile 0
    mem <- MEM.mkMemory memStart memSize
    pure $ ArchState reg mem

-- | Write a textual representation of the t'ArchState' to standard output.
dumpState :: ArchState -> IO ()
dumpState ArchState{getReg=r} =
    REG.dumpRegs (showHex . fromIntegral @Int32 @Word32) r >>= putStr

-- | Implements concrete interpretation of the 'Operations' effect based on a 'BV' value representation.
defaultInstructions :: MonadIO m => ArchState -> Operations BV ~> m
defaultInstructions (ArchState regFile mem) = liftIO . \case
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
