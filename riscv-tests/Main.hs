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
import Control.Monad (when)
import Control.Monad.Freer

import Loader
import Spec.AST
import Spec.Expr
import CmdLine
import Effects.Logging.InstructionFetch
import Machine.Standard.Interpreter
import Common.Types

import qualified Machine.Standard.Register as REG
import qualified Machine.Standard.Memory as MEM

runTestInstructionM :: forall r effs . LastMember IO effs => (Expr Word32 -> Word32) -> ArchState -> Eff (Instruction Word32 ': effs) r -> Eff effs r
runTestInstructionM evalE (regFile, mem) = interpretM $ \case
    (ReadRegister idx) -> fromIntegral <$> REG.readRegister regFile idx
    (WriteRegister idx reg) -> REG.writeRegister regFile idx (fromIntegral $ evalE reg)
    (LoadByte addr) -> fromIntegral <$> MEM.loadByte mem (evalE addr)
    (LoadHalf addr) -> fromIntegral <$> (MEM.loadHalf mem (evalE addr) :: IO (Word16))
    (LoadWord addr) -> MEM.loadWord mem (evalE addr)
    (StoreByte addr w) -> MEM.storeByte mem (evalE addr) (fromIntegral $ evalE w)
    (StoreHalf addr w) -> MEM.storeHalf mem (evalE addr) (fromIntegral (evalE w) :: Word16)
    (StoreWord addr w) -> MEM.storeWord mem (evalE addr) (evalE w)
    (WritePC w) -> REG.writePC regFile (evalE w)
    ReadPC -> REG.readPC regFile
    (Ecall pc) -> do
        sys <- REG.readRegister regFile A7
        arg <- REG.readRegister regFile A0

        when (sys /= 93) $
            fail "unknown syscall"

        if arg == 0
            then (putStrLn "All tests passed!") >> exitWith (ExitFailure 42)
            else (putStrLn $ "Test" ++ show arg ++ " failed!") >> exitWith (ExitFailure 1)
    (Ebreak pc) -> putStrLn $ "ebreak at 0x" ++ showHex pc ""
    LiftE e -> pure $ evalE e

------------------------------------------------------------------------

main' :: BasicArgs -> IO ()
main' (BasicArgs memAddr memSize trace putReg fp) = do
    state <- mkArchState memAddr memSize
    entry <- loadExecutable fp state

    -- Let stack pointer start at end of memory by default.
    let initalSP = fromIntegral $ memAddr + memSize

    let interpreter =
            if trace then
                runTestInstructionM runExpression state . runLogInstructionFetchM
            else
                runTestInstructionM runExpression state . runNoLogging
    runM $ interpreter $ buildAST entry initalSP

    when putReg $
        dumpState state

main :: IO ()
main = main' =<< execParser opts
    where
        opts = info (basicArgs <**> helper)
            ( fullDesc
           <> progDesc "Concrete execution of RV32I machine code")
