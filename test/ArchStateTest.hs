module ArchStateTest where

import Test.Tasty
import Test.Tasty.HUnit

import Common.Types (RegIdx(..))
import Machine.Standard.Memory
import Machine.Standard.Register

import qualified Data.ByteString.Lazy as BSL

registerTests = testGroup "RegisterFile Tests"
  [ testCase "Read and write general-puropose register" $ do
      r <- mkRegFile
      writeRegister r A1 23
      regVal <- readRegister r A1
      assertEqual "" 23 regVal

  , testCase "Write zero register" $ do
      r <- mkRegFile
      writeRegister r Zero 42
      regVal <- readRegister r Zero
      assertEqual "" 0 regVal

  , testCase "Read and write program counter" $ do
      r <- mkRegFile
      initVal <- readPC r
      assertEqual "PC is zero initially" 0 initVal

      writePC r 1337
      regVal <- readPC r
      assertEqual "PC can be written" 1337 regVal

      writePC r 0
      regVal <- readPC r
      assertEqual "Zero can be written to PC" 0 regVal
  ]

memoryTests = testGroup "Memory Tests"
  [ testCase "Create memory and extract its size" $ do
      m <- mkMemory 0x0 512
      memSize m >>= assertEqual "" 512

  , testCase "Store and load byte" $ do
      m <- mkMemory 0x0 512
      storeByte m 0x4 0xab
      loadByte m 0x04 >>= assertEqual "" 0xab

  , testCase "Store and load word" $ do
      m <- mkMemory 0x0 256
      storeWord m 8 0xdeadbeef
      loadWord m 8  >>= assertEqual "Load entire word" 0xdeadbeef
      loadByte m 8  >>= assertEqual "Load 1st byte"    0xde
      loadByte m 9  >>= assertEqual "Load 2nd byte"    0xad
      loadByte m 11 >>= assertEqual "Load 4th byte"    0xef

  , testCase "Write ByteString in little endian byteorder" $ do
      m <- mkMemory 0x0 32
      let bs = BSL.pack [0xde, 0xad, 0xbe, 0xef]

      storeByteString m 0x0 bs
      loadWord m 0x0 >>= assertEqual "" 0xefbeadde
  ]
