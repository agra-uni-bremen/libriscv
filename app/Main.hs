{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
module Main where

import Options.Applicative
import Control.Monad (when)
import Control.Monad.Freer
import Control.Monad.Freer.Reader

import LibRISCV (RegIdx(SP))
import LibRISCV.Utils (align)
import LibRISCV.Loader
import LibRISCV.Spec.AST
import LibRISCV.Spec.Operations
import LibRISCV.CmdLine
import LibRISCV.Effects.Logging.InstructionFetch
import LibRISCV.Machine.Interpreter
import qualified LibRISCV.Spec.Expr as E
import Data.BitVector 
import qualified Debug.Trace as Debug


main' :: BasicArgs -> IO ()
main' (BasicArgs memAddr memSize trace putReg fp) = do
    state <- mkArchState memAddr memSize
    entry <- loadExecutable fp state

    -- Let stack pointer start at end of memory by default.
    -- It must be possible to perform a LW with this address.
    let initalSP = align (memAddr + memSize - 1)

    let interpreter =
            if trace then
                runReader (runExpression, state) . runInstruction defaultBehavior . runLogInstructionFetchM
            else
                runReader (runExpression, state) . runInstruction defaultBehavior . runNoLogging
    runM $ interpreter $ do
        writeRegister (bitVec 32 $ fromEnum SP) (E.FromImm 32 (bitVec 32 initalSP))
        Debug.trace "Starting23" (pure ())
        buildAST @32 (bitVec 32 entry)

    when putReg $
        dumpState state

main :: IO ()
main = main' =<< execParser opts
    where
        opts = info (basicArgs <**> helper)
            ( fullDesc
           <> progDesc "Concrete execution of RV32I machine code")
