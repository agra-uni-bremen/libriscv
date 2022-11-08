module Common.Types where

import Data.Int
import Data.Word
import Data.Ix
import Conversion
import qualified Data.ByteString.Lazy as BSL

type Signed32 = Int32
type Unsigned32 = Word32

-- Type used to represent RISC-V registers.
type Register = Int32

-- 32-bit addresses for RV32.
type Address = Word32

-- | Type to index for the register file.
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

------------------------------------------------------------------------

class ByteAddrsMem a where
    storeByteString :: a -> Address -> BSL.ByteString -> IO ()
