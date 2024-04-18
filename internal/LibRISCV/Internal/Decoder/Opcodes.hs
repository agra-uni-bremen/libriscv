{-# LANGUAGE TemplateHaskell #-}

module LibRISCV.Internal.Decoder.Opcodes where

import Data.Word
import Data.Bits
import Data.BitVector (BV)
import LibRISCV.Internal.Decoder.Generator

generateDefaultDecoder
