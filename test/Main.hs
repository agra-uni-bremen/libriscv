module Main where

import Test.DocTest

main :: IO ()
main = doctest ["app/Decoder.hs",
                "app/Memory.hs"]
