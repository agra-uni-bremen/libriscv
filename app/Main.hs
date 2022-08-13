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
    , trace    :: Bool
    , putRegs  :: Bool
    , file     :: String }

cmdArgs :: Parser CmdArgs
cmdArgs = CmdArgs
    <$> option auto
        ( long "memory-size"
       <> short 's'
       <> value 1024
       <> help "Size of the memory region" )
    <*> switch
        ( long "trace"
       <> short 't'
       <> help "Whether to trace all executed instructions" )
    <*> switch
        ( long "registers"
       <> short 'r'
       <> help "Whether to print all register values" )
    <*> argument str (metavar "FILE")

main' :: CmdArgs -> IO ()
main' (CmdArgs memSize trace putReg fp) = do
    mem <- mkMemory memoryStart $ fromIntegral memSize

    entry <- loadExecutable fp mem
    state <- mkArchState mem

    executeAll state tracer entry

    if putReg
        then do out <- dumpRegs $ fst state
                putStr out
        else pure ()

    where
        tracer = if trace
            then Just $ MkDebugTracer stdout
            else Nothing

main :: IO ()
main = main' =<< execParser opts
    where
        opts = info (cmdArgs <**> helper)
            ( fullDesc
           <> progDesc "Execute RV32I machine code"
           <> header "riscv-tiny - a simulator for RV32I" )
