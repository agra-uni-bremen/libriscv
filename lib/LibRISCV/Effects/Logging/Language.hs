{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}

-- | Implements the logging effect for instruction tracing. This effect
-- is primarly intended to be used for debugging.
module LibRISCV.Effects.Logging.Language where

import Control.Monad.Freer.TH (makeEffect)
import LibRISCV.Internal.Decoder.Opcodes (InstructionType)

data LogInstructionFetch r where
    -- TODO: Log program counter (i.e. instruction address) too.
    LogFetched :: InstructionType -> LogInstructionFetch ()

makeEffect ''LogInstructionFetch
