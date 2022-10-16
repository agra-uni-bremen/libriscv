{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
module Interpreter.Logging.InstructionFetch where

import Control.Monad.Free
import Types (Address)
import Decoder (Instruction)
import Numeric (showHex)
import Common.Coproduct
import Common.Effects.Noop

data LogInstructionFetch r = 
    LogFetch Address Instruction r deriving Functor 

logFetch :: (LogInstructionFetch :<: f) => Address -> Instruction -> Free f ()
logFetch a i = inject $ LogFetch a i $ Pure ()

-- default log interpreter
evalIO :: LogInstructionFetch (IO a) -> IO a
evalIO (LogFetch addr inst next) = do
    putStrLn $ showHex addr $ ": " ++ show inst
    next

instance Noop LogInstructionFetch where 
    noop (LogFetch _ _ next) = next