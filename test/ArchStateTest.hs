module ArchStateTest where

import Test.Tasty
import Test.Tasty.HUnit

import Data.Word
import Common.Types (RegIdx(..))
import Machine.Standard.Memory
import Machine.Standard.Register

import qualified Data.ByteString.Lazy as BSL

mkReg :: IO (RegisterFile Word32)
mkReg = mkRegFile 0

registerTests = testGroup "RegisterFile Tests"
  [ testCase "Read and write general-puropose register" $ do
      r <- mkReg
      writeRegister r A1 23
      regVal <- readRegister r A1
      assertEqual "" 23 regVal

  , testCase "Write zero register" $ do
      r <- mkReg
      writeRegister r Zero 42
      regVal <- readRegister r Zero
      assertEqual "" 0 regVal

  , testCase "Read and write program counter" $ do
      r <- mkReg
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

  , testCase "StoreWord in between" $ do
      m <- mkMemory 0x0 12 :: IO (Memory Word8)
      storeWord m 0 (0xffffffff :: Word32)
      storeWord m 4 (0xffffffff :: Word32)
      storeWord m 8 (0xffffffff :: Word32)
      storeWord m 0x2 (0x12345678 :: Word32)
      (loadWord m 0 :: IO Word32) >>= assertEqual "1st word" 0xffff1234
      (loadWord m 4 :: IO Word32) >>= assertEqual "2nd word" 0x5678ffff
      (loadWord m 8 :: IO Word32) >>= assertEqual "3rd word" 0xffffffff

  , testCase "Store and load word" $ do
      m <- mkMemory 0x0 256 :: IO (Memory Word8)
      storeWord m 8 (0xdeadbeef :: Word32)
      (loadWord m 8 :: IO Word32) >>= assertEqual "Load entire word" 0xdeadbeef
      loadByte m 8  >>= assertEqual "Load 1st byte"    0xde
      loadByte m 9  >>= assertEqual "Load 2nd byte"    0xad
      loadByte m 11 >>= assertEqual "Load 4th byte"    0xef

  , testCase "Write ByteString in little endian byteorder" $ do
      m <- mkMemory 0x0 32 :: IO (Memory Word8)
      let bs = BSL.pack [0xde, 0xad, 0xbe, 0xef]

      storeByteString m 0x0 bs
      (loadWord m 0x0 :: IO Word32) >>= assertEqual "" 0xefbeadde

  , testCase "Write ByteString with multiple bytes" $ do
      m <- mkMemory 0x0 8 :: IO (Memory Word8)
      let bs = BSL.pack [0xde, 0xad, 0xbe, 0xef, 0x12, 0x23, 0x34, 0xff]

      storeByteString m 0x0 bs
      (loadWord m 0x0 :: IO Word32) >>= assertEqual "" 0xefbeadde
      (loadWord m 0x4 :: IO Word32) >>= assertEqual "" 0xff342312
  ]
