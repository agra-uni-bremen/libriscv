{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
module Effects.Logging.InstructionFetch where

import Common.Types (Address)
import Decoder (InstructionType)
import Numeric (showHex)
import Control.Monad.Freer
import Control.Monad.Freer.TH

data LogInstructionFetch r where
    LogFetched :: Address -> InstructionType -> LogInstructionFetch ()

makeEffect ''LogInstructionFetch

-- default log interpreter
runLogInstructionFetchM :: forall effs r . LastMember IO effs => Eff (LogInstructionFetch ': effs) r -> Eff effs r
runLogInstructionFetchM = interpretM $ \case
    LogFetched addr inst -> putStrLn $ showHex addr $ ": " ++ show inst

runNoLogging :: forall effs r . Eff (LogInstructionFetch ': effs) r -> Eff effs r
runNoLogging = interpret $ \case
    LogFetched _ _ -> pure ()
