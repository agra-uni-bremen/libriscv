module Main where

import Data.Word
import System.Environment ()
import Options.Applicative
import Control.Monad (when)
import Control.Monad.Freer
import Control.Monad.Freer.Reader

import LibRISCV
import LibRISCV.Loader
import LibRISCV.Spec.AST
import LibRISCV.Spec.Expr
import LibRISCV.Spec.Instruction
import LibRISCV.CmdLine
import LibRISCV.Effects.Logging.InstructionFetch
import LibRISCV.Machine.Standard.Interpreter (runInstruction)

import Interpreter

data TaintArgs = TaintArgs
    { taintRegister :: RegIdx
    , base :: BasicArgs }

taintArgs :: Parser TaintArgs
taintArgs = TaintArgs
    <$> option auto
        ( long "taint-register"
       <> help "Taint the given register" )
    <*> basicArgs

main' :: TaintArgs -> IO ()
main' (TaintArgs taintReg (BasicArgs memAddr memSize trace putReg fp)) = do
    state <- mkArchState memAddr memSize
    entry <- loadExecutable fp state

    -- Let stack pointer start at end of memory by default.
    let initalSP = fromIntegral $ memAddr + memSize

    let interpreter =
            if trace then
                runReader (runExpression, state) . runInstruction iftBehavior . runLogInstructionFetchM
            else
                runReader (runExpression, state) . runInstruction iftBehavior . runNoLogging
    runM $ interpreter $ do
        writeRegister taintReg (FromImm $ MkTainted True (0 :: Word32))
        buildAST (MkTainted False (entry :: Word32)) (MkTainted False (initalSP :: Word32))

    when putReg $
        dumpState state

main :: IO ()
main = main' =<< execParser opts
    where
        opts = info (taintArgs <**> helper)
            ( fullDesc
           <> progDesc "Concrete execution of RV32I machine code")
