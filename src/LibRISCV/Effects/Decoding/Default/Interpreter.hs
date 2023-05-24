{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module LibRISCV.Effects.Decoding.Default.Interpreter where
import LibRISCV.Effects.Decoding.Language ( Decoding(..) )
import Data.Data (Proxy(..))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.IORef (IORef, writeIORef, readIORef)
import Data.Word (Word32)
import Data.BitVector (BV, bitVec)
import Control.Monad.Freer ( type (~>) )
import LibRISCV.Decoder.Opcode (decode)
import LibRISCV.Decoder.Instruction (mkRd, mkRs1, mkRs2, immI, immS, immB, immU, immJ, mkShamt)


type DefaultDecodingEnv = IORef Word32

defaultDecoding :: MonadIO m => DefaultDecodingEnv -> Decoding BV ~> m
defaultDecoding instRef = 
    let 
        decodeAndConvert f = fmap (bitVec 32 . f) . readIORef 
    in liftIO . \case
        SetInstr v -> writeIORef instRef $ fromIntegral v
        WithInstrType Proxy f -> f . decode <$> readIORef instRef
        DecodeRD -> decodeAndConvert mkRd instRef
        DecodeRS1 -> decodeAndConvert mkRs1 instRef 
        DecodeRS2 -> decodeAndConvert mkRs2 instRef
        DecodeImmI -> decodeAndConvert immI instRef
        DecodeImmS -> decodeAndConvert immS instRef 
        DecodeImmB -> decodeAndConvert immB instRef 
        DecodeImmU -> decodeAndConvert immU instRef 
        DecodeImmJ -> decodeAndConvert immJ instRef 
        DecodeShamt -> decodeAndConvert mkShamt instRef 