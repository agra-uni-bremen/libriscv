module Main where

import Tracer
import Loader
import Memory
import Executor
import Register
import System.Environment
import GHC.IO.StdHandles
import Options.Applicative

-- Address at which the memory is supposed to be mapped.
memoryStart :: Address
memoryStart = 0x10000

data CmdArgs = CmdArgs
    { memStart :: Int
    , verbose  :: Bool
    , file     :: String }

cmdArgs :: Parser CmdArgs
cmdArgs = CmdArgs
    <$> option auto
        ( long "memory-size"
       <> short 's'
       <> value 1024
       <> help "Size of the memory region" )
    <*> switch
        ( long "verbose"
       <> short 'v'
       <> help "Whether to print executed instructions" )
    <*> argument str (metavar "FILE")

main' :: CmdArgs -> IO ()
main' (CmdArgs memSize ver fp) = do
    mem <- mkMemory memoryStart $ fromIntegral memSize

    entry <- loadExecutable fp mem
    state <- mkArchState mem

    putStrLn "\nExecuting all instructions…"
    executeAll state (MkDebugTracer stdout) entry

    if ver
        then do putStrLn "\nDumping register file…"
                out <- dumpRegs $ fst state
                putStr out
        else pure ()

main :: IO ()
main = main' =<< execParser opts
    where
        opts = info (cmdArgs <**> helper)
            ( fullDesc
           <> progDesc "Execute RV32I machine code"
           <> header "riscv-tiny - a simulator for RV32I" )
