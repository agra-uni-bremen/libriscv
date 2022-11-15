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
import System.Environment ()
import System.Exit
import Options.Applicative
import Control.Monad (when, mzero)
import Control.Monad.Freer
import Control.Monad.Freer.Reader (runReader)

import Loader
import Spec.AST
import Spec.Expr
import Spec.Instruction
import CmdLine
import Effects.Logging.InstructionFetch
import Machine.Standard.Interpreter
import Common.Types
import Common.Utils
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Class ( MonadTrans(lift) ) 

import qualified Machine.Standard.Register as REG
import qualified Machine.Standard.Memory as MEM


testSpecificEcallBehavior :: DefaultEnv -> Instruction Word32 ~> MaybeT IO
testSpecificEcallBehavior (evalE, (regFile, mem)) = \case
        Ecall pc -> do
            sys <- lift $ REG.readRegister regFile A7
            arg <- lift $ REG.readRegister regFile A0

            when (sys /= 93) $
                fail "unknown syscall"

            lift $ if arg == 0
                then putStrLn "All tests passed!" >> exitWith (ExitFailure 42) 
                else putStrLn ("Test" ++ show arg ++ " failed!") >> exitWith (ExitFailure 1)
        _ -> mzero


main' :: BasicArgs -> IO ()
main' (BasicArgs memAddr memSize trace putReg fp) = do
    state <- mkArchState memAddr memSize
    entry <- loadExecutable fp state

    -- Let stack pointer start at end of memory by default.
    let initalSP = fromIntegral $ memAddr + memSize

    let interpreter =
            if trace then
                runReader (runExpression, state) . runInstruction (testSpecificEcallBehavior `extends` defaultBehavior) . runLogInstructionFetchM 
            else
                runReader (runExpression, state) . runInstruction (testSpecificEcallBehavior `extends` defaultBehavior) . runNoLogging
    runM $ interpreter $ buildAST entry initalSP

    when putReg $
        dumpState state

main :: IO ()
main = main' =<< execParser opts
    where
        opts = info (basicArgs <**> helper)
            ( fullDesc
           <> progDesc "Concrete execution of RV32I machine code")
