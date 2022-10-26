import Test.Tasty
import Test.Tasty.HUnit

import DecoderTest
import ArchStateTest

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [
    decoderTests
  , registerTests
  , memoryTests
  ]
