{-# LANGUAGE DeriveGeneric #-}

module LibRISCV.Internal.Decoder.YamlParser where

import qualified Data.Map.Strict as M
import Data.Yaml (FromJSON)
import GHC.Generics (Generic)

data InstructionFields = InstructionFields
    { extension :: [String]
    , mask :: String
    , match :: String
    }
    deriving (Eq, Show, Generic)

instance FromJSON InstructionFields

type InstructionEntries = M.Map String InstructionFields
