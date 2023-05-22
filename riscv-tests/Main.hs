{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
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
import Data.IORef (newIORef)

import LibRISCV
import LibRISCV.Loader
import LibRISCV.Semantics.Default
import LibRISCV.CmdLine
import LibRISCV.Effects.Logging.Default.Interpreter
import LibRISCV.Effects.Operations.Default.Interpreter
import LibRISCV.Effects.Operations.Language
import qualified LibRISCV.Effects.Expressions.Expr as E
import LibRISCV.Effects.Expressions.Default.Interpreter
import LibRISCV.Effects.Expressions.Default.EvalE 
import LibRISCV.Effects.Decoding.Default.Interpreter
import LibRISCV.Utils
import LibRISCV.Effects.Operations.Default.Machine.Memory (storeByteString)

import Control.Monad.IO.Class ( MonadIO(..) )
import qualified LibRISCV.Effects.Operations.Default.Machine.Register as REG
import Data.BitVector (BV, bitVec)

-- Syscall number for the newlib exit syscall (used by riscv-tests).
sys_exit :: Int32
sys_exit = 93

type ECallEnv = DefaultInstructionsEnv

-- The riscv-tests repository uses a special ecall to communicate test
-- failures to the execution environment. This function implements the
-- ECALL instruction accordingly.
ecallHandler :: ECallEnv -> Operations BV ~> IO
ecallHandler env@(regFile, mem) = \case
        Ecall pc -> do
            sys <- liftIO $ REG.readRegister regFile A7
            arg <- liftIO $ REG.readRegister regFile A0

            when (sys /= sys_exit) $
                fail "unknown syscall"

            liftIO $ if arg == 0
                then exitSuccess
                else exitWith (ExitFailure $ fromIntegral arg)
        x -> defaultInstructions env x


main' :: BasicArgs -> IO ()
main' (BasicArgs memAddr memSize trace putReg fp) = do
    state@(_, mem) <- mkArchState memAddr memSize

    elf <- readElf fp
    loadElf elf $ storeByteString mem
    entry <- startAddr elf

    instRef <- newIORef (0 :: Word32)
    let 
        evalEnv         = ((==1), evalE)
        interpreter =
                interpretM (ecallHandler state) . 
                interpretM (defaultEval evalEnv) . 
                interpretM (defaultDecoding instRef) . 
                interpretM (if trace then defaultLogging else noLogging)
    runM $ interpreter $ buildAST @32 (bitVec 32 entry)

    when putReg $
        dumpState state

main :: IO ()
main = main' =<< execParser opts
    where
        opts = info (basicArgs <**> helper)
            ( fullDesc
           <> progDesc "Concrete execution of RV32I machine code")
