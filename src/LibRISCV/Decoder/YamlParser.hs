{-# LANGUAGE DeriveGeneric #-}
module LibRISCV.Decoder.YamlParser where

import Data.Word (Word32)
import GHC.Generics ( Generic )
import Data.Yaml (FromJSON (parseJSON), withObject)
import qualified Data.Map.Strict as M

data InstructionFields = InstructionFields 
    { extension :: [String] 
    , mask :: String
    , match :: String 
    } deriving (Eq, Show, Generic)

instance FromJSON InstructionFields

type InstructionEntries = M.Map String InstructionFields