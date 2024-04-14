{-# LANGUAGE TemplateHaskell #-}

module LibRISCV.Decoder.Opcodes where

import Data.Word
import Data.Bits
import Data.BitVector (BV)
import LibRISCV.Decoder.Generator

generateDefaultDecoder
