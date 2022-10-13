{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Interpreter.Logging.InstructionFetch where

import Control.Monad.Free
import Control.Monad.Free.TH
import Types (Address)
import Decoder (Instruction)
import Numeric (showHex)

data LogInstructionFetch r = 
    LogInstructionFetch Address Instruction r deriving Functor 

type LogInstructionFetchM = Free LogInstructionFetch

makeFree ''LogInstructionFetch

-- default log interpreter
runIO :: LogInstructionFetch (IO a) -> IO a
runIO (LogInstructionFetch addr inst next) = do
    putStrLn $ showHex addr $ ": " ++ show inst
    next