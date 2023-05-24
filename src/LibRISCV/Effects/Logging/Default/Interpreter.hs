{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
module LibRISCV.Effects.Logging.Default.Interpreter where
import Control.Monad.IO.Class ( MonadIO(..) )
import LibRISCV.Effects.Logging.Language
    ( LogInstructionFetch(..) )
import Control.Monad.Freer ( type (~>) )
import Numeric (showHex)


defaultLogging :: MonadIO m => LogInstructionFetch ~> m
defaultLogging = liftIO . \case
    LogFetched addr inst -> putStrLn $ showHex addr $ ": " ++ show inst

noLogging :: Monad m => LogInstructionFetch ~> m
noLogging = \case
    LogFetched _ _ -> pure ()
