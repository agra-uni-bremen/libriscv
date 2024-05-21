import Test.Tasty

import DecoderTest
import ArchStateTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [
    decoderTests
  , registerTests
  , memoryTests
  ]
