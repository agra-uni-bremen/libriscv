{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad (when)
import Control.Monad.Freer
import Data.BitVector
import Data.IORef (newIORef)
import Data.Word (Word32)
import LibRISCV (RegIdx (SP), align)
import LibRISCV.CmdLine
import LibRISCV.Effects.Decoding.Default.Interpreter (
    defaultDecoding,
 )
import LibRISCV.Effects.Expressions.Default.Interpreter (defaultEval, evalE)
import qualified LibRISCV.Effects.Expressions.Expr as E
import LibRISCV.Effects.Logging.Default.Interpreter (
    defaultLogging,
    noLogging,
 )
import LibRISCV.Effects.Operations.Default.Interpreter (
    defaultInstructions,
    dumpState,
    getMem,
    mkArchState,
 )
import LibRISCV.Effects.Operations.Default.Machine.Memory (storeByteString)
import LibRISCV.Loader
import LibRISCV.Semantics (buildAST, writeRegister)
import Options.Applicative

main' :: BasicArgs -> IO ()
main' (BasicArgs memAddr memSize trace putReg fp) = do
    state <- mkArchState memAddr memSize

    elf <- readElf fp
    loadElf elf $ storeByteString fromIntegral (getMem state)
    entry <- startAddr elf

    instRef <- newIORef (0 :: Word32)
    let
        initalSP = align (memAddr + memSize - 1)
        evalEnv = ((== 1), evalE)
        interpreter =
            interpretM (defaultInstructions state)
                . interpretM (defaultEval evalEnv)
                . interpretM (defaultDecoding @BV instRef)
                . interpretM (if trace then defaultLogging else noLogging)
    runM $ interpreter $ do
        writeRegister (bitVec 32 $ fromEnum SP) (E.FromInt 32 $ fromIntegral initalSP)
        buildAST @32 (bitVec 32 entry)

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
