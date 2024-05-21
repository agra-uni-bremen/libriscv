{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- | An implementation of an /Executable Loadable Format/ (ELF) loader. The module
-- is responsible for loading instructions into a provided memory implementation
-- and obtaining the entry point for the executable.
module LibRISCV.Loader (readElf, LoadFunc, loadElf, startAddr) where

import Control.Monad.Catch (MonadCatch)
import Data.Bits ()
import qualified Data.ByteString.Lazy as BSL
import Data.Elf (
    Elf (..),
    ElfListXX (..),
    ElfNodeType (..),
    ElfSectionData (ElfSectionData),
    ElfXX (
        ElfSection,
        ElfSegment,
        ehEntry,
        epAddMemSize,
        epData,
        epType,
        esAddr,
        esData
    ),
    elfFindHeader,
    parseElf,
 )
import Data.Elf.Constants
import Data.Elf.Headers (
    SingElfClass (SELFCLASS32, SELFCLASS64),
    SingElfClassI,
    withSingElfClassI,
 )
import Data.Elf.PrettyPrint (readFileLazy)
import Data.Int (Int64)
import Data.Word (Word32)
import LibRISCV
import LibRISCV.Utils ()
import System.FilePath ()

-- Filter all ELF segments with type PT_LOAD.
loadableSegments :: ElfListXX a -> [ElfXX 'Segment a]
loadableSegments (ElfListCons v@(ElfSegment{..}) l) =
    if epType == PT_LOAD
        then v : loadableSegments l
        else loadableSegments l
loadableSegments (ElfListCons _ l) = loadableSegments l
loadableSegments ElfListNull = []

-- Copy data from ElfSection to memory at the given absolute address.
copyData :: (Monad m, SingElfClassI a) => ElfListXX a -> Int64 -> LoadFunc m -> m ()
copyData ElfListNull _ _ = pure ()
copyData (ElfListCons (ElfSection{esData = ElfSectionData textData, ..}) xs) zeros f = do
    f (fromIntegral esAddr) $ BSL.append textData (BSL.replicate zeros 0)
    copyData xs zeros f
copyData (ElfListCons _ xs) zeros f = copyData xs zeros f

-- Load an ElfSegment into memory at the given address.
loadSegment :: (Monad m, SingElfClassI a) => LoadFunc m -> ElfXX 'Segment a -> m ()
loadSegment loadFunc ElfSegment{..} =
    copyData epData (fromIntegral epAddMemSize) loadFunc

------------------------------------------------------------------------

-- | Load a 'BSL.ByteString' into memory at a given address.
type LoadFunc m = Address -> BSL.ByteString -> m ()

-- | Load all loadable segments of an ELF file into memory. An addition to the
-- 'Data.Elf.Elf' file, it requires an implementation of a 'LoadFunc' which is
-- responsible for converting a 'BSL.ByteString' to the internal value
-- representation.
loadElf :: (Monad m) => Elf -> LoadFunc m -> m ()
loadElf (Elf classS elfs) loadFunc = withSingElfClassI classS $ do
    let loadable = loadableSegments elfs
    mapM_ (loadSegment loadFunc) loadable

-- | Read an ELF from a given 'FilePath'.
readElf :: FilePath -> IO Elf
readElf path = readFileLazy path >>= parseElf

-- | Return the entry point from the ELF header.
startAddr :: (MonadCatch m) => Elf -> m Word32
startAddr (Elf SELFCLASS32 elfs) = ehEntry <$> elfFindHeader elfs
startAddr (Elf SELFCLASS64 _) = error "64-bit executables not supported"
