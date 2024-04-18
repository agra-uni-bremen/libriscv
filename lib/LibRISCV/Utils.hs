module LibRISCV.Utils where

import LibRISCV

import Data.Word ( Word32 )
import Control.Monad (when,unless)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Control.Monad.Trans.Class (lift)

-- Convert bool to a Word32.
boolToWord :: Bool -> Word32
boolToWord True  = 1
boolToWord False = 0

-- Like Control.Monad.when but interprets given Word32 as a bool.
whenMword :: Monad m => m Word32 -> m () -> m ()
whenMword mb act = mb >>= flip when act . (==1)

-- Like Control.Monad.unless but interprets given Word32 as a bool.
unlessMword :: Monad m => m Word32 -> m () -> m ()
unlessMword mb act = mb >>= flip unless act . (==1)
