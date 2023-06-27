{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
module LibRISCV.Effects.Logging.Language where

import LibRISCV (Address)
import LibRISCV.Decoder.Opcodes (InstructionType)
import Control.Monad.Freer
import Control.Monad.Freer.TH ( makeEffect )

data LogInstructionFetch r where
    -- TODO: Log program counter (i.e. instruction address) too.
    LogFetched :: InstructionType -> LogInstructionFetch ()

makeEffect ''LogInstructionFetch
