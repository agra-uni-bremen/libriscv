{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad (mzero, when)
import Control.Monad.Freer
import Control.Monad.Freer.Reader (runReader)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Maybe
import Data.BitVector (BV, bitVec)
import Data.IORef (newIORef)
import Data.Int
import Data.Word
import LibRISCV
import LibRISCV.CmdLine
import LibRISCV.Effects.Decoding.Default.Interpreter
import LibRISCV.Effects.Expressions.Default.Interpreter
import qualified LibRISCV.Effects.Expressions.Expr as E
import LibRISCV.Effects.Logging.Default.Interpreter
import LibRISCV.Effects.Operations.Default.Interpreter
import LibRISCV.Effects.Operations.Default.Machine.Memory (storeByteString)
import qualified LibRISCV.Effects.Operations.Default.Machine.Register as REG
import LibRISCV.Effects.Operations.Language
import LibRISCV.Loader
import LibRISCV.Semantics (buildAST, readRegister)
import Numeric
import Options.Applicative
import System.Environment ()
import System.Exit

-- Syscall number for the newlib exit syscall (used by riscv-tests).
sys_exit :: Int32
sys_exit = 93

-- The riscv-tests repository uses a special ecall to communicate test
-- failures to the execution environment. This function implements the
-- ECALL instruction accordingly.
ecallHandler :: ArchState -> Operations BV ~> IO
ecallHandler env = \case
    Ecall pc -> do
        sys <- liftIO $ REG.readRegister (getReg env) A7
        arg <- liftIO $ REG.readRegister (getReg env) A0

        when (sys /= sys_exit) $
            fail "unknown syscall"

        liftIO $
            if arg == 0
                then exitSuccess
                else exitWith (ExitFailure $ fromIntegral arg)
    x -> defaultInstructions env x

main' :: BasicArgs -> IO ()
main' (BasicArgs memAddr memSize trace putReg fp) = do
    state <- mkArchState memAddr memSize

    elf <- readElf fp
    loadElf elf $ storeByteString fromIntegral (getMem state)
    entry <- startAddr elf

    instRef <- newIORef (0 :: Word32)
    let
        evalEnv = ((== 1), evalE)
        interpreter =
            interpretM (ecallHandler state)
                . interpretM (defaultEval evalEnv)
                . interpretM (defaultDecoding @BV instRef)
                . interpretM (if trace then defaultLogging else noLogging)
    runM $ interpreter $ buildAST @32 (bitVec 32 entry)

    when putReg $
        dumpState state

main :: IO ()
main = main' =<< execParser opts
  where
    opts =
        info
            (basicArgs <**> helper)
            ( fullDesc
                <> progDesc "Concrete execution of RV32I machine code"
            )
