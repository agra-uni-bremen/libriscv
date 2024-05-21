{-# LANGUAGE MultiParamTypeClasses #-}
module LibRISCV where

import Data.Word
import Data.Ix

-- | Representation of a 32-bit addresses for RV32.
type Address = Word32

-- | Align an 'Address' on the next word boundary.
align :: Address -> Address
align addr = addr - addr `mod` 4

-- | Type to represent an index for the register file.
--
-- >>> toEnum 12 :: RegIdx
-- A2
-- >>> toEnum 31 :: RegIdx
-- T6
-- >>> fromEnum (minBound :: RegIdx)
-- 0
--
data RegIdx = Zero | RA | SP | GP | TP | T0 | T1 | T2 | FP
    | S1 | A0 | A1 | A2 | A3 | A4 | A5 | A6 | A7 | S2 | S3
    | S4 | S5 | S6 | S7 | S8 | S9 | S10 | S11 | T3 | T4
    | T5 | T6 deriving (Ord, Eq, Ix, Bounded, Show, Enum, Read)
