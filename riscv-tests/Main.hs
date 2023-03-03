{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
module Main where

import Numeric
import Data.Word
import Data.Int
import System.Environment ()
import System.Exit
import Options.Applicative
import Control.Monad (when, mzero)
import Control.Monad.Freer
import Control.Monad.Freer.Reader (runReader)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class ( MonadTrans(lift) )

import LibRISCV
import LibRISCV.Utils (align)
import LibRISCV.Loader
import LibRISCV.Spec.AST
import LibRISCV.Spec.Expr
import LibRISCV.Spec.Operations
import LibRISCV.CmdLine
import LibRISCV.Effects.Logging.InstructionFetch
import LibRISCV.Machine.Interpreter
import LibRISCV.Utils

import qualified LibRISCV.Machine.Register as REG
import qualified LibRISCV.Machine.Memory as MEM

-- Syscall number for the newlib exit syscall (used by riscv-tests).
sys_exit :: Int32
sys_exit = 93

-- The riscv-tests repository uses a special ecall to communicate test
-- failures to the execution environment. This function implements the
-- ECALL instruction accordingly.
ecallHandler :: DefaultEnv -> Operations Word32 ~> MaybeT IO
ecallHandler (evalE, (regFile, mem)) = \case
        Ecall pc -> do
            sys <- lift $ REG.readRegister regFile A7
            arg <- lift $ REG.readRegister regFile A0

            when (sys /= sys_exit) $
                fail "unknown syscall"

            lift $ if arg == 0
                then exitWith ExitSuccess
                else exitWith (ExitFailure $ fromIntegral arg)
        _ -> mzero


main' :: BasicArgs -> IO ()
main' (BasicArgs memAddr memSize trace putReg fp) = do
    state <- mkArchState memAddr memSize
    entry <- loadExecutable fp state

    let interpreter =
            if trace then
                runReader (runExpression, state) . runInstruction (ecallHandler `extends` defaultBehavior) . runLogInstructionFetchM
            else
                runReader (runExpression, state) . runInstruction (ecallHandler `extends` defaultBehavior) . runNoLogging
    runM $ interpreter $ buildAST entry

    when putReg $
        dumpState state

main :: IO ()
main = main' =<< execParser opts
    where
        opts = info (basicArgs <**> helper)
            ( fullDesc
           <> progDesc "Concrete execution of RV32I machine code")
