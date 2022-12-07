module DecoderTest where

import Test.Tasty
import Test.Tasty.HUnit

import Decoder
import Common.Types

decoderTests = testGroup "Decoder Tests"
  [ testCase "Decode ADD instruction" $
      assertEqual "" ADD { rd=A1, rs1=A2, rs2=A0 } $ decode 0x00a605b3

  , testCase "Decode ADDI instruction" $
      assertEqual "" ADDI { rd=T0, rs1=T1, imm=42 } $ decode 0x02a30293

  , testCase "Decode LW instruction" $
      assertEqual "" LW { rd=A0, rs1=A0, imm=(-4) } $ decode 0xffc52503

  , testCase "Decode AUIPC instruction" $
      assertEqual "" AUIPC { rd=T0, imm=0 } $ decode 0x00000297

  , testCase "Decode SW instruction" $
      assertEqual "" SW { rs1=Zero, rs2=RA, imm=8 } $ decode 0x00102423

  , testCase "Decode JAL instruction" $
      assertEqual "" JAL { rd=Zero, imm=(-4) } $ decode 0xffdff06f

  , testCase "Decode BLT instruction" $
      assertEqual "" BLT { rs1=A0, rs2=A1, imm=4 } $ decode 0x00b54263

  , testCase "Decode SLLI instruction" $
      assertEqual "" SLLI { rd=A1, rs1=T0, shamt=1} $ decode 0x00129593

  , testCase "Decode SLLI instruction" $
      assertEqual "" SRLI { rd=A3, rs1=T0, shamt=31} $ decode 0x01f2d693
  ]
