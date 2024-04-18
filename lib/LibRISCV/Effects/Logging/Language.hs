{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

-- | Implements the logging effect for instruction tracing. This effect
-- is primarly intended to be used for debugging.
module LibRISCV.Effects.Logging.Language where

import LibRISCV (Address)
import LibRISCV.Internal.Decoder.Opcodes (InstructionType)
import Control.Monad.Freer
import Control.Monad.Freer.TH ( makeEffect )

data LogInstructionFetch r where
    -- TODO: Log program counter (i.e. instruction address) too.
    LogFetched :: InstructionType -> LogInstructionFetch ()

makeEffect ''LogInstructionFetch
