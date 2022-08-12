module Main where

import Test.DocTest

main :: IO ()
main = doctest ["app/Register.hs",
                "app/Decoder.hs",
                "app/Memory.hs",
                "app/Utils.hs"]
