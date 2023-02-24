--
-- !! THIS FILE IS AUTO-GENERATED !!
--

module LibRISCV.Decoder.Opcode
    (InstructionType(..), decode)
where

import Data.Word
import Data.Bits

data InstructionType =
  ADD |
  ADDI |
  AND |
  ANDI |
  AUIPC |
  BEQ |
  BGE |
  BGEU |
  BLT |
  BLTU |
  BNE |
  EBREAK |
  ECALL |
  FENCE |
  JAL |
  JALR |
  LB |
  LBU |
  LH |
  LHU |
  LUI |
  LW |
  OR |
  ORI |
  SB |
  SH |
  SLL |
  SLLI |
  SLT |
  SLTI |
  SLTIU |
  SLTU |
  SRA |
  SRAI |
  SRL |
  SRLI |
  SUB |
  SW |
  XOR |
  XORI |
  InvalidInstruction deriving (Eq, Show)

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
ebreak_mask = 0xffffffff
ebreak_match = 0x100073
ecall_mask = 0xffffffff
ecall_match = 0x73
fence_mask = 0x707f
fence_match = 0xf
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
  | instrWord .&. add_mask == add_match = ADD
  | instrWord .&. addi_mask == addi_match = ADDI
  | instrWord .&. and_mask == and_match = AND
  | instrWord .&. andi_mask == andi_match = ANDI
  | instrWord .&. auipc_mask == auipc_match = AUIPC
  | instrWord .&. beq_mask == beq_match = BEQ
  | instrWord .&. bge_mask == bge_match = BGE
  | instrWord .&. bgeu_mask == bgeu_match = BGEU
  | instrWord .&. blt_mask == blt_match = BLT
  | instrWord .&. bltu_mask == bltu_match = BLTU
  | instrWord .&. bne_mask == bne_match = BNE
  | instrWord .&. ebreak_mask == ebreak_match = EBREAK
  | instrWord .&. ecall_mask == ecall_match = ECALL
  | instrWord .&. fence_mask == fence_match = FENCE
  | instrWord .&. jal_mask == jal_match = JAL
  | instrWord .&. jalr_mask == jalr_match = JALR
  | instrWord .&. lb_mask == lb_match = LB
  | instrWord .&. lbu_mask == lbu_match = LBU
  | instrWord .&. lh_mask == lh_match = LH
  | instrWord .&. lhu_mask == lhu_match = LHU
  | instrWord .&. lui_mask == lui_match = LUI
  | instrWord .&. lw_mask == lw_match = LW
  | instrWord .&. or_mask == or_match = OR
  | instrWord .&. ori_mask == ori_match = ORI
  | instrWord .&. sb_mask == sb_match = SB
  | instrWord .&. sh_mask == sh_match = SH
  | instrWord .&. sll_mask == sll_match = SLL
  | instrWord .&. slli_mask == slli_match = SLLI
  | instrWord .&. slt_mask == slt_match = SLT
  | instrWord .&. slti_mask == slti_match = SLTI
  | instrWord .&. sltiu_mask == sltiu_match = SLTIU
  | instrWord .&. sltu_mask == sltu_match = SLTU
  | instrWord .&. sra_mask == sra_match = SRA
  | instrWord .&. srai_mask == srai_match = SRAI
  | instrWord .&. srl_mask == srl_match = SRL
  | instrWord .&. srli_mask == srli_match = SRLI
  | instrWord .&. sub_mask == sub_match = SUB
  | instrWord .&. sw_mask == sw_match = SW
  | instrWord .&. xor_mask == xor_match = XOR
  | instrWord .&. xori_mask == xori_match = XORI
  | True = InvalidInstruction
