{-# LANGUAGE BinaryLiterals #-}

module Decoder where

import Data.Int
import Data.Bits
import Data.Word
import Register

-- Types used to represent immediates.
type Iimm = Int32 -- XXX: Technically 12-bits
type Simm = Iimm
type Bimm = Int32 -- XXX: Technically 12-bits
type Uimm = Int32 -- XXX: Technically 20-bits
type Jimm = Uimm

-- Type used to represent a decoded RISC-V instruction.
data Instruction =
    Add   RegIdx RegIdx RegIdx |
    And   RegIdx RegIdx RegIdx |
    Andi  Iimm RegIdx RegIdx |
    Addi  Iimm RegIdx RegIdx |
    Lw    Iimm RegIdx RegIdx |
    Sw    Simm RegIdx RegIdx |
    Jal   Jimm RegIdx |
    Lui   RegIdx Uimm |
    Auipc RegIdx Uimm |
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

immS :: Word32 -> Simm
immS i = fromTwoscomp 12 $ fromIntegral $
    (instrField 25 31 i `shift` 5) .|.  (instrField 07 11 i)

immB :: Word32 -> Bimm
immB i = fromTwoscomp 13 $
        ((instrField 31 31 i `shift` 12)
     .|. (instrField 07 07 i `shift` 11)
     .|. (instrField 25 30 i `shift` 05)
     .|. (instrField 08 11 i `shift` 01))

immU :: Word32 -> Uimm
immU i = fromIntegral $ instrField 12 31 i `shiftL` 12

immJ :: Word32 -> Jimm
immJ i = fromTwoscomp 21 $
        ((instrField 31 31 i `shift` 20)
     .|. (instrField 12 19 i `shift` 12)
     .|. (instrField 20 20 i `shift` 11)
     .|. (instrField 21 30 i `shift` 1))

rs1 :: Word32 -> RegIdx
rs1 = toEnum . fromIntegral . instrField 15 19

rs2 :: Word32 -> RegIdx
rs2 = toEnum . fromIntegral . instrField 20 24

rd :: Word32 -> RegIdx
rd = toEnum . fromIntegral . instrField 7 11

------------------------------------------------------------------------

-- Opcodes
op_reg   = 0b0110011
op_imm   = 0b0010011
op_load  = 0b0000011
op_store = 0b0100011
op_jal   = 0b1101111
op_lui   = 0b0110111
op_auipc = 0b0010111

-- Funct3 for register-immediate instructions.
f3_addi = 0b000
f3_andi = 0b111

-- Funct3 for register-register instructions.
f3_add = 0b000
f3_and = 0b111

-- Funct3 for load instructions.
f3_loadw = 0b010

-- Funct3 for store instructions.
f3_storew = 0b010

-- Type for an R-Type instruction (three register operands).
type RTypeInstr = (RegIdx -> RegIdx -> RegIdx -> Instruction)

invalid_rtype :: RTypeInstr
invalid_rtype _ _ _ = InvalidInstruction

-- Type for an I-Type instruction (two register, one immediate).
type ITypeInstr = (Iimm -> RegIdx -> RegIdx -> Instruction)

invalid_itype :: ITypeInstr
invalid_itype _ _ _ = InvalidInstruction

-- Type for an S-Type instruction (two register, one S-Immediate).
type STypeInstr = (Simm -> RegIdx -> RegIdx -> Instruction)

invalid_stype :: STypeInstr
invalid_stype _ _ _ = InvalidInstruction

------------------------------------------------------------------------

-- Decode integer register-register instructions.
decode_reg :: Word32 -> RTypeInstr
decode_reg instr
    | f3 == f3_add = Add
    | f3 == f3_and = And
    | otherwise    = invalid_rtype
    where
        f3 = funct3 instr

-- Decode integer register-immediate instructions.
decode_imm :: Word32 -> ITypeInstr
decode_imm instr
    | f3 == f3_addi = Addi
    | f3 == f3_andi = Andi
    | otherwise = invalid_itype
    where
        f3 = funct3 instr

-- Decode load instructions.
decode_load :: Word32 -> ITypeInstr
decode_load instr
    | f3 == f3_loadw = Lw
    where
        f3 = funct3 instr

-- Decode store instructions.
decode_store :: Word32 -> STypeInstr
decode_store instr
    | f3 == f3_storew = Sw
    where
        f3 = funct3 instr

decode' :: Word32 -> Word32 -> Instruction
decode' instr opcode
    | opcode == op_reg   = decode_reg instr (rd instr) (rs1 instr) (rs2 instr)
    | opcode == op_imm   = decode_imm instr (immI instr) (rd instr) (rs1 instr)
    | opcode == op_load  = decode_load instr (immI instr) (rd instr) (rs1 instr)
    | opcode == op_store = decode_store instr (immS instr) (rs1 instr) (rs2 instr)
    | opcode == op_jal   = Jal (immJ instr) (rd instr)
    | opcode == op_lui   = Lui (rd instr) (immU instr)
    | opcode == op_auipc = Auipc (rd instr) (immU instr)
    | otherwise          = InvalidInstruction

-- | Decode a RISC-V RV32i instruction.
--
-- Examples:
--
-- >>> decode 0x00a605b3
-- Add A1 A2 A0
-- >>> decode 0x02a30293
-- Addi 42 T0 T1
-- >>> decode 0xffc52503
-- Lw (-4) A0 A0
-- >>> decode 0x00000297
-- Auipc T0 0
-- >>> decode 0x00102423
-- Sw 8 Zero RA
-- >>> decode 0xffdff06f
-- Jal (-4) Zero
--
decode :: Word32 -> Instruction
decode instr = decode' instr $ opcode instr
