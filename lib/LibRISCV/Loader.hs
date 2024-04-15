{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module LibRISCV.Loader (readElf, loadElf, startAddr) where

import LibRISCV
import Control.Monad.IO.Class (MonadIO, liftIO)
import LibRISCV.Utils ()
import Control.Monad.Catch ( MonadCatch )
import Data.Bits ()
import Data.Int ( Int64 )
import Data.Word ( Word32 )
import Data.Elf
    ( elfFindHeader,
      parseElf,
      Elf(..),
      ElfListXX(..),
      ElfNodeType(..),
      ElfSectionData(ElfSectionData),
      ElfXX(ElfSection, ElfSegment, esData, ehEntry,
            esAddr, epType, epAddMemSize, epData)
    )
import Data.Elf.Headers
    ( withSingElfClassI, SingElfClassI, SingElfClass(SELFCLASS32, SELFCLASS64) )
import Data.Elf.Constants 
import Data.Elf.PrettyPrint (readFileLazy)
import qualified Data.ByteString.Lazy as BSL
import System.FilePath ()
import Debug.Trace (trace)

-- Load a ByteString into memory at a given address.
type LoadFunc m = Address -> BSL.ByteString -> m ()

-- Filter all ELF segments with type PT_LOAD.
loadableSegments :: ElfListXX a -> [ElfXX 'Segment a]
loadableSegments (ElfListCons v@(ElfSegment { .. }) l) =
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

-- Load all loadable segments of an ELF file into memory.
loadElf :: (Monad m) => Elf -> LoadFunc m -> m ()
loadElf (Elf classS elfs) loadFunc = withSingElfClassI classS $ do
    let loadable = loadableSegments elfs
    mapM_ (loadSegment loadFunc) loadable

-- Read ELF from given file.
readElf :: FilePath -> IO Elf
readElf path = readFileLazy path >>= parseElf

-- Return the entry point from the ELF header.
startAddr :: MonadCatch m => Elf -> m Word32
startAddr (Elf SELFCLASS32 elfs) = ehEntry <$> elfFindHeader elfs
startAddr (Elf SELFCLASS64 _) = error "64-bit executables not supported"
