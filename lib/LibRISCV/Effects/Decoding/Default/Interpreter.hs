{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

-- | Provides the default (concrete) interpretation for the decoding effect. This
-- implementation assumes that the 'Decodable' type class can be implemented for
-- the underlying type parameter, that is, it must be possible to convert the
-- type to a fixed-width concrete integer.
module LibRISCV.Effects.Decoding.Default.Interpreter where

import Control.Monad.Freer (type (~>))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.BitVector (BV, bitVec)
import Data.Data (Proxy (..))
import Data.IORef (IORef, readIORef, writeIORef)
import Data.Word (Word32)
import LibRISCV.Effects.Decoding.Language (Decoding (..))
import LibRISCV.Internal.Decoder.Instruction (immB, immI, immJ, immS, immU, mkRd, mkRs1, mkRs2, mkShamt)
import LibRISCV.Internal.Decoder.Opcodes (decode)

-- | Decoder state used to implement the stateful 'SetInstr' constructor of the 'Decoding' effect.
type DecoderState = IORef Word32

-- | Type class used to perform conversion from/to a fixed-with concrete integer.
class Decodable a where
    fromWord :: Word32 -> a
    -- ^ Convert from a fixed-with integer to the underlying value type of the interpreter.

    toWord :: a -> Word32
    -- ^ Convert from the underlying value type to a fixed-with integer.

instance Decodable BV where
    fromWord = bitVec 32
    toWord = fromIntegral

-- | Concrete implementation of the decoding effect.
defaultDecoding :: forall v m. (Decodable v, MonadIO m) => DecoderState -> Decoding v ~> m
defaultDecoding instRef =
    let
        decodeAndConvert f = fmap (fromWord . f) . readIORef
     in
        liftIO . \case
            SetInstr v -> writeIORef instRef $ toWord v
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
