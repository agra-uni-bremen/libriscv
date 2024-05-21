module DecoderTest where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Int

import LibRISCV
import LibRISCV.Internal.Decoder.Opcodes
import LibRISCV.Internal.Decoder.Instruction

decoderTests :: TestTree
decoderTests = testGroup "Decoder Tests"
  [ testCase "Decode ADD instruction" $ do
      let inst = 0x00a605b3

      assertEqual "opcode" (RV_I ADD) $ decode inst
      assertEqual "rd"  A1 $ toEnum $ fromIntegral $ mkRd inst
      assertEqual "rs1" A2 $ toEnum $ fromIntegral $ mkRs1 inst
      assertEqual "rs2" A0 $ toEnum $ fromIntegral $ mkRs2 inst

  , testCase "Decode ADDI instruction" $ do
      let inst = 0x02a30293

      assertEqual "opcode" (RV_I ADDI) $ decode inst
      assertEqual "rd"   T0 $ toEnum $ fromIntegral $ mkRd inst
      assertEqual "rs1"  T1 $ toEnum $ fromIntegral $ mkRs1 inst
      assertEqual "immI" 42 $ immI inst

  , testCase "Decode LW instruction" $ do
      let inst = 0xffc52503

      assertEqual "opcode" (RV_I LW) $ decode inst
      assertEqual "rd"   A0   $ toEnum $ fromIntegral $ mkRd inst
      assertEqual "rs1"  A0   $ toEnum $ fromIntegral $ mkRs1 inst
      assertEqual "immI" (-4) (fromIntegral (immI inst) :: Int32)

  , testCase "Decode AUIPC instruction" $ do
      let inst = 0x00000297

      assertEqual "opcode" (RV_I AUIPC) $ decode inst
      assertEqual "rd"   T0 (toEnum $ fromIntegral (mkRd inst))
      assertEqual "immU" 0  (immU inst)

  , testCase "Decode SW instruction" $ do
      let inst = 0x00102423

      assertEqual "opcode" (RV_I SW) $ decode inst
      assertEqual "rs1"  Zero (toEnum $ fromIntegral (mkRs1 inst))
      assertEqual "rs2"  RA   (toEnum $ fromIntegral (mkRs2 inst))
      assertEqual "immS" 8    $ immS inst

  , testCase "Decode JAL instruction" $ do
      let inst = 0xffdff06f

      assertEqual "opcode" (RV_I JAL) $ decode inst
      assertEqual "rd"   Zero (toEnum $ fromIntegral (mkRd inst))
      assertEqual "immJ" (-4) (fromIntegral (immJ inst) :: Int32)

  , testCase "Decode BLT instruction" $ do
      let inst = 0x00b54263

      assertEqual "opcode" (RV_I BLT) $ decode inst
      assertEqual "rs1" A0 (toEnum $ fromIntegral (mkRs1 inst))
      assertEqual "rs2" A1 (toEnum $ fromIntegral (mkRs2 inst))
      assertEqual "immB" 4 $ immB inst

  , testCase "Decode SLLI instruction" $ do
      let inst = 0x00129593

      assertEqual "opcode" (RV32_I SLLI) $ decode inst
      assertEqual "rd"    A1 (toEnum $ fromIntegral (mkRd inst))
      assertEqual "rs1"   T0 (toEnum $ fromIntegral (mkRs1 inst))
      assertEqual "shamt" 01 $ mkShamt inst

  , testCase "Decode SLLI instruction" $ do
      let inst = 0x01f2d693

      assertEqual "opcode" (RV32_I SRLI) $ decode inst
      assertEqual "rd"    A3 (toEnum $ fromIntegral (mkRd inst))
      assertEqual "rs1"   T0 (toEnum $ fromIntegral (mkRs1 inst))
      assertEqual "shamt" 31 $ mkShamt inst
  ]
