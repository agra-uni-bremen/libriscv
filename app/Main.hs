module Main where

import Data.Word
import Tracer
import Loader
import Memory
import Executor
import Register
import System.Environment
import GHC.IO.StdHandles
import Options.Applicative

data CmdArgs = CmdArgs
    { memAddr  :: Word32
    , memStart :: Word32
    , trace    :: Bool
    , putRegs  :: Bool
    , file     :: String }

cmdArgs :: Parser CmdArgs
cmdArgs = CmdArgs
    <$> option auto
        ( long "memory-start"
       <> short 'm'
       <> value 0x10000 )
    <*> option auto
        ( long "memory-size"
       <> short 's'
       <> value (1024 * 1024 * 1) -- 1 MB RAM
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
main' (CmdArgs memAddr memSize trace putReg fp) = do
    mem <- mkMemory memAddr memSize

    entry <- loadExecutable fp mem
    state <- mkArchState mem entry

    executeAll state tracer

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
