module Main where

import Loader
import Data.Word ( Word32 )
import System.Environment ()
import GHC.IO.StdHandles ( stdout )
import Options.Applicative
    ( (<**>),
      argument,
      auto,
      fullDesc,
      header,
      help,
      info,
      long,
      metavar,
      option,
      progDesc,
      short,
      str,
      switch,
      value,
      execParser,
      helper,
      Parser )
import Control.Monad (when)
import Control.Monad.Freer
import Effects.Logging.InstructionFetch
import Effects.Machine.Instruction
import Descriptions.Standard.AST
import Common.Types (Address)
import Effects.Machine.Expression (runExpression)

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
    state <- mkArchState memAddr memSize
    entry <- loadExecutable fp state

    -- Let stack pointer start at end of memory by default.
    let initalSP = fromIntegral $ memAddr + memSize

    let interpreter =
            if trace then
                runInstructionM runExpression state . runLogInstructionFetchM
            else
                runInstructionM runExpression state . runNoLogging
    runM $ interpreter $ buildAST entry initalSP

    when putReg $
        dumpState state

main :: IO ()
main = main' =<< execParser opts
    where
        opts = info (cmdArgs <**> helper)
            ( fullDesc
           <> progDesc "Execute RV32I machine code"
           <> header "riscv-tiny - a simulator for RV32I" )
