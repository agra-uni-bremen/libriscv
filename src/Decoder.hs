--
-- !! THIS FILE IS AUTO-GENERATED !!
--

module Decoder where

import Data.Ix
import Data.Int
import Data.Word
import Data.Bits
import Common.Types (RegIdx)

type Immediate = Int32

data InstructionType =
  ADD { rd :: RegIdx, rs1 :: RegIdx, rs2 :: RegIdx } |
  ADDI { rd :: RegIdx, rs1 :: RegIdx, imm :: Immediate } |
  AUIPC { rd :: RegIdx, imm :: Immediate } |
  BLT { rs1 :: RegIdx, rs2 :: RegIdx, imm :: Immediate } |
  JAL { rd :: RegIdx, imm :: Immediate } |
  JALR { rd :: RegIdx, rs1 :: RegIdx, imm :: Immediate } |
  LUI { rd :: RegIdx, imm :: Immediate } |
  LW { rd :: RegIdx, rs1 :: RegIdx, imm :: Immediate } |
  SW { rs1 :: RegIdx, rs2 :: RegIdx, imm :: Immediate } |
  InvalidInstruction deriving (Eq,Show)

------------------------------------------------------------------------

-- Convert to an unsigned word (in two's complement) to a signed number.
fromTwoscomp :: Word32 -> Word32 -> Int32
fromTwoscomp numBits n = fromIntegral $ -(n .&. mask) + (n .&. complement mask)
    where
        mask :: Word32
        mask = 2^(numBits - 1)

-- Extract a bit field from a RISC-V instruction word.
instrField :: Int -> Int -> Word32 -> Word32
instrField start end w = mask start (end + 1) .&. shiftR w start
    where
        -- Create a 32-bit bit mask in the range [start,end-1].
        mask :: Int -> Int -> Word32
        mask start end = complement $ shift (maxBound :: Word32) (end - start)

------------------------------------------------------------------------

immI :: Word32 -> Immediate
immI = fromIntegral . fromTwoscomp 12 . instrField 20 31

immS :: Word32 -> Immediate
immS i = fromTwoscomp 12 $ fromIntegral $
    (instrField 25 31 i `shift` 5) .|.  instrField 07 11 i

immB :: Word32 -> Immediate
immB i = fromTwoscomp 13 $
         (instrField 31 31 i `shift` 12)
     .|. (instrField 07 07 i `shift` 11)
     .|. (instrField 25 30 i `shift` 05)
     .|. (instrField 08 11 i `shift` 01)

immU :: Word32 -> Immediate
immU i = fromIntegral $ instrField 12 31 i `shiftL` 12

immJ :: Word32 -> Immediate
immJ i = fromTwoscomp 21 $
        (instrField 31 31 i `shift` 20)
     .|. (instrField 12 19 i `shift` 12)
     .|. (instrField 20 20 i `shift` 11)
     .|. (instrField 21 30 i `shift` 1)

mkRs1 :: Word32 -> RegIdx
mkRs1 = toEnum . fromIntegral . instrField 15 19

mkRs2 :: Word32 -> RegIdx
mkRs2 = toEnum . fromIntegral . instrField 20 24

mkRd :: Word32 -> RegIdx
mkRd = toEnum . fromIntegral . instrField 7 11

------------------------------------------------------------------------

----
-- Match and mask constants taken from the riscv-opcodes repository
-- For details, see <https://github.com/riscv/riscv-opcodes>.
----

add_mask = 0xfe00707f
add_match = 0x33
addi_mask = 0x707f
addi_match = 0x13
auipc_mask = 0x7f
auipc_match = 0x17
blt_mask = 0x707f
blt_match = 0x4063
jal_mask = 0x7f
jal_match = 0x6f
jalr_mask = 0x707f
jalr_match = 0x67
lui_mask = 0x7f
lui_match = 0x37
lw_mask = 0x707f
lw_match = 0x2003
sw_mask = 0x707f
sw_match = 0x2023

------------------------------------------------------------------------

decode :: Word32 -> InstructionType
decode instrWord
  | instrWord .&. add_mask == add_match = ADD { rd=mkRd instrWord, rs1=mkRs1 instrWord, rs2=mkRs2 instrWord }
  | instrWord .&. addi_mask == addi_match = ADDI { rd=mkRd instrWord, rs1=mkRs1 instrWord, imm=immI instrWord }
  | instrWord .&. auipc_mask == auipc_match = AUIPC { rd=mkRd instrWord, imm=immU instrWord }
  | instrWord .&. blt_mask == blt_match = BLT { rs1=mkRs1 instrWord, rs2=mkRs2 instrWord, imm=immB instrWord }
  | instrWord .&. jal_mask == jal_match = JAL { rd=mkRd instrWord, imm=immJ instrWord }
  | instrWord .&. jalr_mask == jalr_match = JALR { rd=mkRd instrWord, rs1=mkRs1 instrWord, imm=immI instrWord }
  | instrWord .&. lui_mask == lui_match = LUI { rd=mkRd instrWord, imm=immU instrWord }
  | instrWord .&. lw_mask == lw_match = LW { rd=mkRd instrWord, rs1=mkRs1 instrWord, imm=immI instrWord }
  | instrWord .&. sw_mask == sw_match = SW { rs1=mkRs1 instrWord, rs2=mkRs2 instrWord, imm=immS instrWord }
  | True = InvalidInstruction
