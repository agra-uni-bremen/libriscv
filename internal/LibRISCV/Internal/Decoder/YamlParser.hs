{-# LANGUAGE DeriveGeneric #-}
module LibRISCV.Internal.Decoder.YamlParser where

import GHC.Generics (Generic)
import Data.Yaml (FromJSON)
import qualified Data.Map.Strict as M

data InstructionFields = InstructionFields
    { extension :: [String]
    , mask :: String
    , match :: String
    } deriving (Eq, Show, Generic)

instance FromJSON InstructionFields

type InstructionEntries = M.Map String InstructionFields
