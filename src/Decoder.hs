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
  AND { rd :: RegIdx, rs1 :: RegIdx, rs2 :: RegIdx } |
  ANDI { rd :: RegIdx, rs1 :: RegIdx, imm :: Immediate } |
  AUIPC { rd :: RegIdx, imm :: Immediate } |
  BEQ { rs1 :: RegIdx, rs2 :: RegIdx, imm :: Immediate } |
  BGE { rs1 :: RegIdx, rs2 :: RegIdx, imm :: Immediate } |
  BGEU { rs1 :: RegIdx, rs2 :: RegIdx, imm :: Immediate } |
  BLT { rs1 :: RegIdx, rs2 :: RegIdx, imm :: Immediate } |
  BLTU { rs1 :: RegIdx, rs2 :: RegIdx, imm :: Immediate } |
  BNE { rs1 :: RegIdx, rs2 :: RegIdx, imm :: Immediate } |
  JAL { rd :: RegIdx, imm :: Immediate } |
  JALR { rd :: RegIdx, rs1 :: RegIdx, imm :: Immediate } |
  LB { rd :: RegIdx, rs1 :: RegIdx, imm :: Immediate } |
  LBU { rd :: RegIdx, rs1 :: RegIdx, imm :: Immediate } |
  LH { rd :: RegIdx, rs1 :: RegIdx, imm :: Immediate } |
  LHU { rd :: RegIdx, rs1 :: RegIdx, imm :: Immediate } |
  LUI { rd :: RegIdx, imm :: Immediate } |
  LW { rd :: RegIdx, rs1 :: RegIdx, imm :: Immediate } |
  OR { rd :: RegIdx, rs1 :: RegIdx, rs2 :: RegIdx } |
  ORI { rd :: RegIdx, rs1 :: RegIdx, imm :: Immediate } |
  SB { rs1 :: RegIdx, rs2 :: RegIdx, imm :: Immediate } |
  SH { rs1 :: RegIdx, rs2 :: RegIdx, imm :: Immediate } |
  SLL { rd :: RegIdx, rs1 :: RegIdx, rs2 :: RegIdx } |
  SLLI { rd :: RegIdx, rs1 :: RegIdx, shamt :: Word32 } |
  SLT { rd :: RegIdx, rs1 :: RegIdx, rs2 :: RegIdx } |
  SLTI { rd :: RegIdx, rs1 :: RegIdx, imm :: Immediate } |
  SLTIU { rd :: RegIdx, rs1 :: RegIdx, imm :: Immediate } |
  SLTU { rd :: RegIdx, rs1 :: RegIdx, rs2 :: RegIdx } |
  SRA { rd :: RegIdx, rs1 :: RegIdx, rs2 :: RegIdx } |
  SRAI { rd :: RegIdx, rs1 :: RegIdx, shamt :: Word32 } |
  SRL { rd :: RegIdx, rs1 :: RegIdx, rs2 :: RegIdx } |
  SRLI { rd :: RegIdx, rs1 :: RegIdx, shamt :: Word32 } |
  SUB { rd :: RegIdx, rs1 :: RegIdx, rs2 :: RegIdx } |
  SW { rs1 :: RegIdx, rs2 :: RegIdx, imm :: Immediate } |
  XOR { rd :: RegIdx, rs1 :: RegIdx, rs2 :: RegIdx } |
  XORI { rd :: RegIdx, rs1 :: RegIdx, imm :: Immediate } |
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

mkShamt :: Word32 -> Word32
mkShamt = instrField 20 25 . fromIntegral . immI

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
and_mask = 0xfe00707f
and_match = 0x7033
andi_mask = 0x707f
andi_match = 0x7013
auipc_mask = 0x7f
auipc_match = 0x17
beq_mask = 0x707f
beq_match = 0x63
bge_mask = 0x707f
bge_match = 0x5063
bgeu_mask = 0x707f
bgeu_match = 0x7063
blt_mask = 0x707f
blt_match = 0x4063
bltu_mask = 0x707f
bltu_match = 0x6063
bne_mask = 0x707f
bne_match = 0x1063
jal_mask = 0x7f
jal_match = 0x6f
jalr_mask = 0x707f
jalr_match = 0x67
lb_mask = 0x707f
lb_match = 0x3
lbu_mask = 0x707f
lbu_match = 0x4003
lh_mask = 0x707f
lh_match = 0x1003
lhu_mask = 0x707f
lhu_match = 0x5003
lui_mask = 0x7f
lui_match = 0x37
lw_mask = 0x707f
lw_match = 0x2003
or_mask = 0xfe00707f
or_match = 0x6033
ori_mask = 0x707f
ori_match = 0x6013
sb_mask = 0x707f
sb_match = 0x23
sh_mask = 0x707f
sh_match = 0x1023
sll_mask = 0xfe00707f
sll_match = 0x1033
slli_mask = 0xfe00707f
slli_match = 0x1013
slt_mask = 0xfe00707f
slt_match = 0x2033
slti_mask = 0x707f
slti_match = 0x2013
sltiu_mask = 0x707f
sltiu_match = 0x3013
sltu_mask = 0xfe00707f
sltu_match = 0x3033
sra_mask = 0xfe00707f
sra_match = 0x40005033
srai_mask = 0xfe00707f
srai_match = 0x40005013
srl_mask = 0xfe00707f
srl_match = 0x5033
srli_mask = 0xfe00707f
srli_match = 0x5013
sub_mask = 0xfe00707f
sub_match = 0x40000033
sw_mask = 0x707f
sw_match = 0x2023
xor_mask = 0xfe00707f
xor_match = 0x4033
xori_mask = 0x707f
xori_match = 0x4013

------------------------------------------------------------------------

decode :: Word32 -> InstructionType
decode instrWord
  | instrWord .&. add_mask == add_match = ADD { rd=mkRd instrWord, rs1=mkRs1 instrWord, rs2=mkRs2 instrWord }
  | instrWord .&. addi_mask == addi_match = ADDI { rd=mkRd instrWord, rs1=mkRs1 instrWord, imm=immI instrWord }
  | instrWord .&. and_mask == and_match = AND { rd=mkRd instrWord, rs1=mkRs1 instrWord, rs2=mkRs2 instrWord }
  | instrWord .&. andi_mask == andi_match = ANDI { rd=mkRd instrWord, rs1=mkRs1 instrWord, imm=immI instrWord }
  | instrWord .&. auipc_mask == auipc_match = AUIPC { rd=mkRd instrWord, imm=immU instrWord }
  | instrWord .&. beq_mask == beq_match = BEQ { rs1=mkRs1 instrWord, rs2=mkRs2 instrWord, imm=immB instrWord }
  | instrWord .&. bge_mask == bge_match = BGE { rs1=mkRs1 instrWord, rs2=mkRs2 instrWord, imm=immB instrWord }
  | instrWord .&. bgeu_mask == bgeu_match = BGEU { rs1=mkRs1 instrWord, rs2=mkRs2 instrWord, imm=immB instrWord }
  | instrWord .&. blt_mask == blt_match = BLT { rs1=mkRs1 instrWord, rs2=mkRs2 instrWord, imm=immB instrWord }
  | instrWord .&. bltu_mask == bltu_match = BLTU { rs1=mkRs1 instrWord, rs2=mkRs2 instrWord, imm=immB instrWord }
  | instrWord .&. bne_mask == bne_match = BNE { rs1=mkRs1 instrWord, rs2=mkRs2 instrWord, imm=immB instrWord }
  | instrWord .&. jal_mask == jal_match = JAL { rd=mkRd instrWord, imm=immJ instrWord }
  | instrWord .&. jalr_mask == jalr_match = JALR { rd=mkRd instrWord, rs1=mkRs1 instrWord, imm=immI instrWord }
  | instrWord .&. lb_mask == lb_match = LB { rd=mkRd instrWord, rs1=mkRs1 instrWord, imm=immI instrWord }
  | instrWord .&. lbu_mask == lbu_match = LBU { rd=mkRd instrWord, rs1=mkRs1 instrWord, imm=immI instrWord }
  | instrWord .&. lh_mask == lh_match = LH { rd=mkRd instrWord, rs1=mkRs1 instrWord, imm=immI instrWord }
  | instrWord .&. lhu_mask == lhu_match = LHU { rd=mkRd instrWord, rs1=mkRs1 instrWord, imm=immI instrWord }
  | instrWord .&. lui_mask == lui_match = LUI { rd=mkRd instrWord, imm=immU instrWord }
  | instrWord .&. lw_mask == lw_match = LW { rd=mkRd instrWord, rs1=mkRs1 instrWord, imm=immI instrWord }
  | instrWord .&. or_mask == or_match = OR { rd=mkRd instrWord, rs1=mkRs1 instrWord, rs2=mkRs2 instrWord }
  | instrWord .&. ori_mask == ori_match = ORI { rd=mkRd instrWord, rs1=mkRs1 instrWord, imm=immI instrWord }
  | instrWord .&. sb_mask == sb_match = SB { rs1=mkRs1 instrWord, rs2=mkRs2 instrWord, imm=immS instrWord }
  | instrWord .&. sh_mask == sh_match = SH { rs1=mkRs1 instrWord, rs2=mkRs2 instrWord, imm=immS instrWord }
  | instrWord .&. sll_mask == sll_match = SLL { rd=mkRd instrWord, rs1=mkRs1 instrWord, rs2=mkRs2 instrWord }
  | instrWord .&. slli_mask == slli_match = SLLI { rd=mkRd instrWord, rs1=mkRs1 instrWord, shamt=mkShamt instrWord }
  | instrWord .&. slt_mask == slt_match = SLT { rd=mkRd instrWord, rs1=mkRs1 instrWord, rs2=mkRs2 instrWord }
  | instrWord .&. slti_mask == slti_match = SLTI { rd=mkRd instrWord, rs1=mkRs1 instrWord, imm=immI instrWord }
  | instrWord .&. sltiu_mask == sltiu_match = SLTIU { rd=mkRd instrWord, rs1=mkRs1 instrWord, imm=immI instrWord }
  | instrWord .&. sltu_mask == sltu_match = SLTU { rd=mkRd instrWord, rs1=mkRs1 instrWord, rs2=mkRs2 instrWord }
  | instrWord .&. sra_mask == sra_match = SRA { rd=mkRd instrWord, rs1=mkRs1 instrWord, rs2=mkRs2 instrWord }
  | instrWord .&. srai_mask == srai_match = SRAI { rd=mkRd instrWord, rs1=mkRs1 instrWord, shamt=mkShamt instrWord }
  | instrWord .&. srl_mask == srl_match = SRL { rd=mkRd instrWord, rs1=mkRs1 instrWord, rs2=mkRs2 instrWord }
  | instrWord .&. srli_mask == srli_match = SRLI { rd=mkRd instrWord, rs1=mkRs1 instrWord, shamt=mkShamt instrWord }
  | instrWord .&. sub_mask == sub_match = SUB { rd=mkRd instrWord, rs1=mkRs1 instrWord, rs2=mkRs2 instrWord }
  | instrWord .&. sw_mask == sw_match = SW { rs1=mkRs1 instrWord, rs2=mkRs2 instrWord, imm=immS instrWord }
  | instrWord .&. xor_mask == xor_match = XOR { rd=mkRd instrWord, rs1=mkRs1 instrWord, rs2=mkRs2 instrWord }
  | instrWord .&. xori_mask == xori_match = XORI { rd=mkRd instrWord, rs1=mkRs1 instrWord, imm=immI instrWord }
  | True = InvalidInstruction
