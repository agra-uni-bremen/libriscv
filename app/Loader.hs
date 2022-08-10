{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Loader (loadExecutable) where

import Utils
import Memory
import Decoder
import Register
import Control.Monad.Catch
import Data.Bits
import Data.Int
import Data.Word
import Data.Elf
import Data.Elf.Headers
import Data.Elf.Constants
import Data.Elf.PrettyPrint (readFileLazy,printElf)
import Data.Singletons
import Data.Singletons.Sigma
import qualified Data.ByteString.Lazy as BSL
import System.FilePath

-- Return the entry point from the ELF header.
startAddr :: Elf -> IO (Word32)
startAddr (SELFCLASS32 :&: ElfList elfs) = do
    hdr <- elfFindHeader elfs
    return $ case hdr of
        ElfHeader {..} -> ehEntry
        _ -> error "no header" -- XXX

-- Return all ELF segments with type PT_LOAD.
loadableSegments :: forall a m . (SingI a, MonadThrow m) => [ElfXX a] -> m [ElfXX a]
loadableSegments elfs = pure $ filter f elfs
    where
        f e@ElfSegment{..} = epType == PT_LOAD
        f _ = False

-- Copy raw data from ELF to memory at the given absolute address.
copyData :: (IsElfClass a) => [ElfXX a] -> Memory -> IO ()
copyData [] _ = pure ()
copyData ((ElfSection{esData = ElfSectionData textData, ..}):xs) mem = do
    storeByteString mem (fromIntegral esAddr) textData
    copyData xs mem
copyData (x:xs) mem = copyData xs mem

-- Load all loadable segments of an ELF file into memory.
loadElf :: Memory -> Elf -> IO (Word32)
loadElf mem elf@(classS :&: ElfList elfs) = withElfClass classS $ do
    loadable <- loadableSegments elfs
    -- TODO: Load zero as specified by epAddMemSize
    mapM (\ElfSegment{..} -> copyData epData mem) loadable
    startAddr elf

-- Read ELF from given file.
readElf :: FilePath -> IO (Elf)
readElf path = do
    bs <- readFileLazy path
    parseElf bs

-- Load executable ELF from file into memory and return entry address.
loadExecutable :: FilePath -> Memory -> IO (Address)
loadExecutable fp mem = readElf fp >>= loadElf mem
