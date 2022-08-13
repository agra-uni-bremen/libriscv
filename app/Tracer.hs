module Tracer where

import Decoder
import Memory (Address)
import Numeric
import GHC.IO.Handle
import GHC.IO.StdHandles

class Tracer a where
    -- TODO: trace :: Monad m => a -> Address -> Instruction -> m ()
    trace :: a -> Address -> Instruction -> IO ()

data DebugTracer = MkDebugTracer Handle

instance Tracer DebugTracer where
    trace (MkDebugTracer h) addr inst = do
        hPutStr h (showHex addr $ ": " ++ (show inst) ++ "\n")
