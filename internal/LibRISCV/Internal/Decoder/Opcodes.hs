{-# LANGUAGE TemplateHaskell #-}

-- generateDefaultDecoder doesn't generate signatures.
{-# OPTIONS_GHC -Wno-missing-signatures #-}

module LibRISCV.Internal.Decoder.Opcodes where

import LibRISCV.Internal.Decoder.Generator

generateDefaultDecoder
