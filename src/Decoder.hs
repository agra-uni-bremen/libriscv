{-# LANGUAGE BinaryLiterals #-}

module Decoder where

import Data.Int ( Int32 )
import Data.Bits ( Bits((.|.), complement, shiftR, (.&.), shiftL, shift) )
import Data.Word ( Word32 )
import Register ( RegIdx )

-- Types used to represent immediates.
newtype Iimm = Iimm { getIimm :: Int32 } deriving Show-- XXX: Technically 12-bits
newtype Simm = Simm { getSimm :: Int32 } deriving Show
newtype Bimm = Bimm { getBimm :: Int32 } deriving Show-- XXX: Technically 12-bits
newtype Uimm = Uimm { getUimm :: Int32 } deriving Show-- XXX: Technically 20-bits
newtype Jimm = Jimm { getJimm :: Int32 } deriving Show

-- Type used to represent a decoded RISC-V instruction.
data Instruction =
    Add   RegIdx RegIdx RegIdx |
    And   RegIdx RegIdx RegIdx |
    Andi  Iimm RegIdx RegIdx |
    Addi  Iimm RegIdx RegIdx |
    Lw    Iimm RegIdx RegIdx |
    Sw    Simm RegIdx RegIdx |
    Blt   Bimm RegIdx RegIdx |
    Jal   Jimm RegIdx |
    Jalr  Iimm RegIdx RegIdx |
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
immI = Iimm . fromIntegral . fromTwoscomp 12 . instrField 20 31

immS :: Word32 -> Simm
immS i = Simm $ fromTwoscomp 12 $ fromIntegral $
    (instrField 25 31 i `shift` 5) .|.  instrField 07 11 i

immB :: Word32 -> Bimm
immB i = Bimm $ fromTwoscomp 13 $
         (instrField 31 31 i `shift` 12)
     .|. (instrField 07 07 i `shift` 11)
     .|. (instrField 25 30 i `shift` 05)
     .|. (instrField 08 11 i `shift` 01)

immU :: Word32 -> Uimm
immU i = Uimm $ fromIntegral $ instrField 12 31 i `shiftL` 12

immJ :: Word32 -> Jimm
immJ i = Jimm $fromTwoscomp 21 $
        (instrField 31 31 i `shift` 20)
     .|. (instrField 12 19 i `shift` 12)
     .|. (instrField 20 20 i `shift` 11)
     .|. (instrField 21 30 i `shift` 1)

rs1 :: Word32 -> RegIdx
rs1 = toEnum . fromIntegral . instrField 15 19

rs2 :: Word32 -> RegIdx
rs2 = toEnum . fromIntegral . instrField 20 24

rd :: Word32 -> RegIdx
rd = toEnum . fromIntegral . instrField 7 11

------------------------------------------------------------------------

-- Opcodes
opReg :: Word32
opReg    = 0b0110011

opImm :: Word32
opImm    = 0b0010011

opLoad :: Word32
opLoad   = 0b0000011

opStore :: Word32
opStore  = 0b0100011

opBranch :: Word32
opBranch = 0b1100011

opJal :: Word32
opJal    = 0b1101111

opJalr :: Word32
opJalr   = 0b1100111

opLui :: Word32
opLui    = 0b0110111

opAuipc :: Word32
opAuipc  = 0b0010111

-- Funct3 for register-immediate instructions.
f3AddI :: Word32
f3AddI = 0b000

f3AndI :: Word32
f3AndI = 0b111

-- Funct3 for register-register instructions.
f3Add :: Word32
f3Add = 0b000

f3And :: Word32
f3And = 0b111

-- Funct3 for load instructions.
f3LoadW :: Word32
f3LoadW = 0b010

-- Funct3 for store instructions.
f3StoreW :: Word32
f3StoreW = 0b010

--- Funct3 for branch instructions.
f3Blt :: Word32
f3Blt = 0b100

-- Type for an R-Type instruction (three register operands).
type RTypeInstr = RegIdx -> RegIdx -> RegIdx -> Instruction

invalidRtype :: RTypeInstr
invalidRtype _ _ _ = InvalidInstruction

-- Type for an I-Type instruction (two register, one immediate).
type ITypeInstr = Iimm -> RegIdx -> RegIdx -> Instruction

invalidItype :: ITypeInstr
invalidItype _ _ _ = InvalidInstruction

-- Type for an S-Type instruction (two register, one S-Immediate).
type STypeInstr = Simm -> RegIdx -> RegIdx -> Instruction

invalidStype :: STypeInstr
invalidStype _ _ _ = InvalidInstruction

-- Type for a B-Type instruction (branches).
type BTypeInstr = Bimm -> RegIdx -> RegIdx -> Instruction

invalidBtype :: BTypeInstr
invalidBtype _ _ _ = InvalidInstruction

------------------------------------------------------------------------

-- Decode integer register-register instructions.
decodeReg :: Word32 -> RTypeInstr
decodeReg instr
    | f3 == f3Add = Add
    | f3 == f3And = And
    | otherwise    = invalidRtype
    where
        f3 = funct3 instr

-- Decode integer register-immediate instructions.
decodeImm :: Word32 -> ITypeInstr
decodeImm instr
    | f3 == f3AddI = Addi
    | f3 == f3AndI = Andi
    | otherwise = invalidItype
    where
        f3 = funct3 instr

-- Decode load instructions.
decodeLoad :: Word32 -> ITypeInstr
decodeLoad instr
    | f3 == f3LoadW = Lw
    where
        f3 = funct3 instr

-- Decode store instructions.
decodeStore :: Word32 -> STypeInstr
decodeStore instr
    | f3 == f3StoreW = Sw
    where
        f3 = funct3 instr

decodeBranch :: Word32 -> BTypeInstr
decodeBranch instr
    | f3 == f3Blt = Blt
    | otherwise = invalidBtype
    where
        f3 = funct3 instr

decode' :: Word32 -> Word32 -> Instruction
decode' instr opcode
    | opcode == opReg    = decodeReg instr (rd instr) (rs1 instr) (rs2 instr)
    | opcode == opImm    = decodeImm instr (immI instr) (rd instr) (rs1 instr)
    | opcode == opLoad   = decodeLoad instr (immI instr) (rd instr) (rs1 instr)
    | opcode == opStore  = decodeStore instr (immS instr) (rs1 instr) (rs2 instr)
    | opcode == opBranch = decodeBranch instr (immB instr) (rs1 instr) (rs2 instr)
    | opcode == opJal    = Jal (immJ instr) (rd instr)
    | opcode == opJalr   = Jalr (immI instr) (rs1 instr) (rd instr)
    | opcode == opLui    = Lui (rd instr) (immU instr)
    | opcode == opAuipc  = Auipc (rd instr) (immU instr)
    | otherwise           = InvalidInstruction

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
-- >>> decode 0x00b54263
-- Blt 4 A0 A1
--
decode :: Word32 -> Instruction
decode instr = decode' instr $ opcode instr
