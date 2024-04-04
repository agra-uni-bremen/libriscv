{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
module LibRISCV.Effects.Logging.Default.Interpreter where
import Control.Monad.IO.Class ( MonadIO(..) )
import LibRISCV.Effects.Logging.Language
    ( LogInstructionFetch(..) )
import Control.Monad.Freer ( type (~>) )


defaultLogging :: MonadIO m => LogInstructionFetch ~> m
defaultLogging = liftIO . \case
    LogFetched inst -> putStrLn $ show inst

noLogging :: Monad m => LogInstructionFetch ~> m
noLogging = \case
    LogFetched _ -> pure ()
