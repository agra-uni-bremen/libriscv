import ArchStateTest
import DecoderTest
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup
        "Tests"
        [ decoderTests
        , registerTests
        , memoryTests
        ]
