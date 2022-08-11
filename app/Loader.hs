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

-- Copy data from ElfSection to memory at the given absolute address.
copyData :: (IsElfClass a) => [ElfXX a] -> Int64 -> Memory -> IO ()
copyData [] _ _ = pure ()
copyData ((ElfSection{esData = ElfSectionData textData, ..}):xs) zeros mem = do
    storeByteString mem (fromIntegral esAddr)
        $ BSL.append textData (BSL.replicate zeros 0)
    copyData xs zeros mem
copyData (x:xs) zeros mem = copyData xs zeros mem

-- Load an ElfSegment into memory at the given address.
loadSegment :: (IsElfClass a) => Memory -> ElfXX a -> IO ()
loadSegment mem ElfSegment{..} =
    copyData epData (fromIntegral epAddMemSize) mem

-- Load all loadable segments of an ELF file into memory.
loadElf :: Memory -> Elf -> IO (Word32)
loadElf mem elf@(classS :&: ElfList elfs) = withElfClass classS $ do
    loadable <- loadableSegments elfs
    mapM (loadSegment mem) loadable
    startAddr elf

-- Read ELF from given file.
readElf :: FilePath -> IO (Elf)
readElf path = do
    bs <- readFileLazy path
    parseElf bs

-- Load executable ELF from file into memory and return entry address.
loadExecutable :: FilePath -> Memory -> IO (Address)
loadExecutable fp mem = readElf fp >>= loadElf mem
