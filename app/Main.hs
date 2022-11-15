module Main where

import System.Environment ()
import Options.Applicative
import Control.Monad (when)
import Control.Monad.Freer
import Control.Monad.Freer.Reader

import Loader
import Spec.AST
import CmdLine
import Effects.Logging.InstructionFetch
import Machine.Standard.Interpreter

main' :: BasicArgs -> IO ()
main' (BasicArgs memAddr memSize trace putReg fp) = do
    state <- mkArchState memAddr memSize
    entry <- loadExecutable fp state

    -- Let stack pointer start at end of memory by default.
    let initalSP = fromIntegral $ memAddr + memSize

    let interpreter =
            if trace then
                runReader (runExpression, state) . runInstruction defaultBehavior . runLogInstructionFetchM
            else
                runReader (runExpression, state) . runInstruction defaultBehavior . runNoLogging
    runM $ interpreter $ buildAST entry initalSP

    when putReg $
        dumpState state

main :: IO ()
main = main' =<< execParser opts
    where
        opts = info (basicArgs <**> helper)
            ( fullDesc
           <> progDesc "Concrete execution of RV32I machine code")
