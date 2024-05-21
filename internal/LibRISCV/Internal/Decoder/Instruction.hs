module LibRISCV.Internal.Decoder.Instruction where

import Data.Word
import Data.Bits

-- Convert to an unsigned word (in two's complement) to a signed number.
fromTwoscomp :: Word32 -> Word32 -> Word32
fromTwoscomp numBits n = fromIntegral $ -(n .&. mask) + (n .&. complement mask)
    where
        mask :: Word32
        mask = 2^(numBits - 1)

-- Extract a bit field from a RISC-V instruction word.
instrField :: Int -> Int -> Word32 -> Word32
instrField start end w = mask start (end + 1) .&. shiftR w start
    where
        -- Create a 32-bit bit mask in the range [start,end-1].
        mask :: Int -> Int -> Word32
        mask start end = complement $ shift (maxBound :: Word32) (end - start)

------------------------------------------------------------------------

immI :: Word32 -> Word32
immI = fromTwoscomp 12 . instrField 20 31

immS :: Word32 -> Word32
immS i = fromTwoscomp 12 $ fromIntegral $
    (instrField 25 31 i `shift` 5) .|.  instrField 07 11 i

immU :: Word32 -> Word32
immU i = instrField 12 31 i `shiftL` 12

{- FOURMOLU_DISABLE -}
immB :: Word32 -> Word32
immB i = fromTwoscomp 13 $
         (instrField 31 31 i `shift` 12)
     .|. (instrField 07 07 i `shift` 11)
     .|. (instrField 25 30 i `shift` 05)
     .|. (instrField 08 11 i `shift` 01)

immJ :: Word32 -> Word32
immJ i = fromTwoscomp 21 $
         (instrField 31 31 i `shift` 20)
     .|. (instrField 12 19 i `shift` 12)
     .|. (instrField 20 20 i `shift` 11)
     .|. (instrField 21 30 i `shift` 1)
{- FOURMOLU_ENABLE -}

mkShamt :: Word32 -> Word32
mkShamt = instrField 20 25

mkRs1 :: Word32 -> Word32
mkRs1 = instrField 15 19

mkRs2 :: Word32 -> Word32
mkRs2 = instrField 20 24

mkRd :: Word32 -> Word32
mkRd = instrField 7 11
