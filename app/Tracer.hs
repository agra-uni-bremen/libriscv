module Tracer where

import Decoder ( Instruction )
import Memory (Address)
import Numeric ( showHex )
import GHC.IO.Handle ( Handle, hPutStr )
import GHC.IO.StdHandles ()

class Tracer a where
    -- TODO: trace :: Monad m => a -> Address -> Instruction -> m ()
    trace :: a -> Address -> Instruction -> IO ()

newtype DebugTracer = MkDebugTracer Handle

instance Tracer DebugTracer where
    trace (MkDebugTracer h) addr inst = 
        hPutStr h (showHex addr $ ": " ++ show inst ++ "\n")
