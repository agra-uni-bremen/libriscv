{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Loader (loadExecutable) where

import Common.Utils ()
import Common.Types
import Decoder ()
import Control.Monad.Catch ( MonadCatch )
import Data.Bits ()
import Data.Int ( Int64 )
import Data.Word ( Word32 )
import Data.Elf
    ( elfFindHeader,
      parseElf,
      Elf,
      ElfList(ElfList),
      ElfSectionData(ElfSectionData),
      ElfXX(ElfSection, ElfSegment, esData, ehEntry,
            esAddr, epType, epAddMemSize, epData)
    )
import Data.Elf.Headers
    ( withElfClass, IsElfClass, SElfClass(SELFCLASS32) )
import Data.Elf.Constants 
import Data.Elf.PrettyPrint (readFileLazy)
import Data.Singletons ( SingI )
import Data.Singletons.Sigma ( Sigma((:&:)) )
import qualified Data.ByteString.Lazy as BSL
import System.FilePath ()

-- Return the entry point from the ELF header.
startAddr :: MonadCatch m => Elf -> m Word32
startAddr (SELFCLASS32 :&: ElfList elfs) = ehEntry <$> elfFindHeader elfs

-- Return all ELF segments with type PT_LOAD.
loadableSegments :: forall a. (SingI a) => [ElfXX a] -> [ElfXX a]
loadableSegments = filter f 
    where
        f ElfSegment{..} = epType == PT_LOAD
        f _ = False

-- Copy data from ElfSection to memory at the given absolute address.
copyData :: (IsElfClass a, ByteAddrsMem m) => [ElfXX a] -> Int64 -> m -> IO ()
copyData [] _ _ = pure ()
copyData ((ElfSection{esData = ElfSectionData textData, ..}):xs) zeros mem = do
    storeByteString mem (fromIntegral esAddr)
        $ BSL.append textData (BSL.replicate zeros 0)
    copyData xs zeros mem
copyData (_:xs) zeros mem = copyData xs zeros mem

-- Load an ElfSegment into memory at the given address.
loadSegment :: (IsElfClass a, ByteAddrsMem m) => m -> ElfXX a -> IO ()
loadSegment mem ElfSegment{..} =
    copyData epData (fromIntegral epAddMemSize) mem

-- Load all loadable segments of an ELF file into memory.
loadElf :: (ByteAddrsMem m) => m -> Elf -> IO Word32
loadElf mem elf@(classS :&: ElfList elfs) = withElfClass classS $ do
    let loadable = loadableSegments elfs
    mapM_ (loadSegment mem) loadable
    startAddr elf

-- Read ELF from given file.
readElf :: FilePath -> IO Elf
readElf path = readFileLazy path >>= parseElf

-- Load executable ELF from file into memory and return entry address.
loadExecutable :: ByteAddrsMem m => FilePath -> m -> IO Address
loadExecutable fp mem = readElf fp >>= loadElf mem
