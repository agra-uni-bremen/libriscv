{-# LANGUAGE TemplateHaskell #-}

module LibRISCV.Decoder.Opcodes where

import Data.Word
import Data.Bits
import Data.BitVector (BV)
import LibRISCV.Decoder.Generator

----
-- Match and mask constants taken from the riscv-opcodes repository
-- For details, see <https://github.com/riscv/riscv-opcodes>.
----

generateDefaultDecoder