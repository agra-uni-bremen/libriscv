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
import LibRISCV.Decoder.Opcode (InstructionType)
import Control.Monad.Freer
import Control.Monad.Freer.TH ( makeEffect )

data LogInstructionFetch r where
    LogFetched :: Address -> InstructionType -> LogInstructionFetch ()

makeEffect ''LogInstructionFetch
