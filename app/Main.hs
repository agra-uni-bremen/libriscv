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
import LibRISCV.Semantics.Default
import LibRISCV.CmdLine
import LibRISCV.Effects.Logging.Default.Interpreter
    ( defaultLogging, noLogging )
import LibRISCV.Effects.Operations.Default.Interpreter
    ( mkArchState, dumpState, defaultInstructions )
import qualified LibRISCV.Effects.Expressions.Expr as E
import LibRISCV.Effects.Expressions.Default.Interpreter (defaultEval)
import LibRISCV.Effects.Expressions.Default.EvalE ( evalE )
import LibRISCV.Effects.Decoding.Default.Interpreter
    ( defaultDecoding )
import Data.BitVector 
import qualified Debug.Trace as Debug
import Data.IORef (newIORef)
import Data.Word (Word32)


main' :: BasicArgs -> IO ()
main' (BasicArgs memAddr memSize trace putReg fp) = do
    state   <- mkArchState memAddr memSize
    entry   <- loadExecutable fp state
    instRef <- newIORef (0 :: Word32)
    -- Let stack pointer start at end of memory by default.
    -- It must be possible to perform a LW with this address.

    let 
        initalSP = align (memAddr + memSize - 1)
        evalEnv = ((==1), evalE)
        interpreter =
                interpretM (defaultInstructions state) . 
                interpretM (defaultEval evalEnv) . 
                interpretM (defaultDecoding instRef) . 
                interpretM (if trace then defaultLogging else noLogging)
    runM $ interpreter $ do
        writeRegister (bitVec 32 $ fromEnum SP) (E.FromInt 32 $ fromIntegral initalSP)
        buildAST @32 (bitVec 32 entry)

    when putReg $
        dumpState state

main :: IO ()
main = main' =<< execParser opts
    where
        opts = info (basicArgs <**> helper)
            ( fullDesc
           <> progDesc "Concrete execution of RV32I machine code")
