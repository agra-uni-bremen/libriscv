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
runIO :: Free LogInstructionFetch a -> IO a
runIO = iterM run' where 
    run' (LogInstructionFetch addr inst next) = do
        putStrLn $ showHex addr $ ": " ++ show inst
        next