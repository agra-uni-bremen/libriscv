{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Implements an effect for instruction decoding. Specifically, the effect
-- is only used to describe obtaining of instruction operands. The instruction
-- opcode is determined seperatly using code generated from the existing
-- <https://github.com/riscv/riscv-opcodes riscv-opcodes> tool. The decoding
-- effect is stateful and operates on a current instruction word specified via
-- 'setInstr', thereby avoiding the need to pass the current instruction word
-- to every decoding function.
module LibRISCV.Effects.Decoding.Language where

import Data.Data (Proxy)
import LibRISCV.Internal.Decoder.Opcodes (InstructionType)
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
    -- TODO: WithInstrType is only used to determine which Haskell module
    -- provides the semantics for a given instruction. Can we eliminate
    -- the need to do this somehow?
    WithInstrType :: Proxy v -> (InstructionType -> b) -> Decoding v b

makeEffect ''Decoding
