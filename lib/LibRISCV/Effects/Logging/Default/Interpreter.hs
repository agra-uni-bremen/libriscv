{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}

-- | Provides the default interpretation for the logging effect.
module LibRISCV.Effects.Logging.Default.Interpreter where

import Control.Monad.Freer (type (~>))
import Control.Monad.IO.Class (MonadIO (..))
import LibRISCV.Effects.Logging.Language (
    LogInstructionFetch (..),
 )

-- | The default effectful logging interpreter which writes the
-- 'LibRISCV.Internal.Decoder.Opcodes.InstructionType' of a fetched instruction
-- to standard output, before executing it. This is particularly useful for
-- debugging but is quite verbose in the common case.
defaultLogging :: (MonadIO m) => LogInstructionFetch ~> m
defaultLogging =
    liftIO . \case
        LogFetched inst -> print inst

-- | A stub implementation of an effectful interpreter which ignores any logging
-- effects entirely. Should be used when no debugging output is desired.
noLogging :: (Monad m) => LogInstructionFetch ~> m
noLogging = \case
    LogFetched _ -> pure ()
