{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module LibRISCV.Effects.Decoding.Language where

import Data.Data (Proxy)
import LibRISCV.Decoder.Opcodes (InstructionType)
import Control.Monad.Freer.TH (makeEffect)


data Decoding v r where
    DecodeRS1 :: Decoding v v
    DecodeRS2 :: Decoding v v
    DecodeRD :: Decoding v v
    DecodeImmI :: Decoding v v
    DecodeImmS :: Decoding v v
    DecodeImmB :: Decoding v v
    DecodeImmU :: Decoding v v
    DecodeImmJ :: Decoding v v
    DecodeShamt :: Decoding v v

    SetInstr :: v -> Decoding v () 
    WithInstrType :: Proxy v -> (InstructionType -> b) -> Decoding v b

makeEffect ''Decoding