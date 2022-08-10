module Main where

import Loader
import Memory
import Executor
import Register
import System.Environment

-- Address at which the memory is supposed to be mapped.
memoryStart :: Address
memoryStart = 0x10000

main :: IO ()
main = do
    args <- getArgs
    if (length args) /= 1
        then error "Accepting only a single file argument"
        else do
            mem  <- mkMemory memoryStart 1024

            entry <- loadExecutable (head args) mem
            state <- mkArchState mem

            putStrLn "\nExecuting all instructions…"
            executeAll state entry

            putStrLn "\nDumping register file…"
            out <- dumpRegs $ fst state
            putStr out
