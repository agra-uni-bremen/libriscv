{-# LANGUAGE BinaryLiterals #-}

module Decoder where

import Data.Int
import Data.Bits
import Data.Word
import Register

-- Type used to represent I-immediates.
type Iimm = Int16 -- XXX: Technically 12-bits

-- Type used to represent a decoded RISC-V instruction.
data Instruction =
    Add  RegIdx RegIdx RegIdx |
    And  RegIdx RegIdx RegIdx |
    Andi Iimm RegIdx RegIdx |
    Addi Iimm RegIdx RegIdx |
    InvalidInstruction deriving (Show)

-- | Convert to an unsigned word to a signed number.
--
-- Examples:
--
-- >>> fromTwoscomp 32 0xffffffff
-- -1
-- >>> fromTwoscomp 3 4
-- -4
-- >>> fromTwoscomp 3 3
-- 3
fromTwoscomp :: Word32 -> Word32 -> Int32
fromTwoscomp numBits n = fromIntegral $ -(n .&. mask) + (n .&. complement mask)
    where
        mask :: Word32
        mask = 2^(numBits - 1)

-- | Extract a bit field from a RISC-V instruction word.
--
-- Examples:
--
-- >>> let word = 0x23421337
-- >>> instrField 24 32 word
-- 35
-- >>> instrField 0 7 word
-- 55
instrField :: Int -> Int -> Word32 -> Word32
instrField start end w = mask start (end + 1) .&. shiftR w start
    where
        -- Create a 32-bit bit mask in the range [start,end-1].
        mask :: Int -> Int -> Word32
        mask start end = complement $ shift (maxBound :: Word32) (end - start)

------------------------------------------------------------------------

opcode :: Word32 -> Word32
opcode = instrField 0 6

funct3 :: Word32 -> Word32
funct3 = instrField 12 14

funct7 :: Word32 -> Word32
funct7 = instrField 25 31

immI :: Word32 -> Iimm
immI = fromIntegral . fromTwoscomp 12 . instrField 20 31

regRs1 :: Word32 -> RegIdx
regRs1 = toEnum . fromIntegral . instrField 15 19

regRs2 :: Word32 -> RegIdx
regRs2 = toEnum . fromIntegral . instrField 20 24

regRd :: Word32 -> RegIdx
regRd = toEnum . fromIntegral . instrField 7 11

------------------------------------------------------------------------

op_rtype = 0b0110011
op_itype = 0b0010011

f3_add = 0b000
f3_and = 0b111

f3_addi = 0b000
f3_andi = 0b111

-- Type for an R-Type instruction (three register operands).
type RTypeInstr = (RegIdx -> RegIdx -> RegIdx -> Instruction)

invalid_rtype :: RTypeInstr
invalid_rtype _ _ _ = InvalidInstruction

decode_rtype :: Word32 -> RTypeInstr
decode_rtype instr
    | f3 == f3_add = Add
    | f3 == f3_and = And
    | otherwise    = invalid_rtype
    where
        f3 = funct3 instr

-- Type for an I-Type instruction (two register, one immediate).
type ITypeInstr = (Iimm -> RegIdx -> RegIdx -> Instruction)

invalid_itype :: ITypeInstr
invalid_itype _ _ _ = InvalidInstruction

decode_itype :: Word32 -> ITypeInstr
decode_itype instr
    | f3 == f3_addi = Addi
    | f3 == f3_andi = Andi
    | otherwise = invalid_itype
    where
        f3 = funct3 instr

decode' :: Word32 -> Word32 -> Instruction
decode' instr opcode
    | opcode == op_rtype = decode_rtype instr (regRd instr) (regRs1 instr) (regRs2 instr)
    | opcode == op_itype = decode_itype instr (immI instr) (regRd instr) (regRs1 instr)
    | otherwise          = InvalidInstruction

-- | Decode a RISC-V RV32i instruction.
--
-- Examples:
--
-- >>> decode 0x00a605b3
-- Add 11 12 10
-- >>> decode 0x02a30293
-- Addi 42 5 6
decode :: Word32 -> Instruction
decode instr = decode' instr $ opcode instr
