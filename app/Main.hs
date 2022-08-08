{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Utils
import Memory
import Decoder
import Executor
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
import System.Environment

-- Address at which the memory is supposed to be mapped.
memoryStart :: Address
memoryStart = 0x10000

-- Translate an ELF physical address to a memory address.
toMemAddr :: Word32 -> Address
toMemAddr = (-) memoryStart

readElf :: FilePath -> IO (Elf)
readElf path = do
    bs <- readFileLazy path
    parseElf bs

startAddr :: Elf -> IO (Maybe Word32)
startAddr (SELFCLASS32 :&: ElfList elfs) = do
    hdr <- elfFindHeader elfs
    return $ case hdr of
        ElfHeader {..} -> Just ehEntry
        _ -> Nothing

-- Return all ELF segments with type PT_LOAD.
loadableSegments :: forall a m . (SingI a, MonadThrow m) => [ElfXX a] -> m [ElfXX a]
loadableSegments elfs = pure $ filter f elfs
    where
        f e@ElfSegment{..} = epType == PT_LOAD
        f _ = False

-- Copy raw data from ELF to memory at the given absolute address.
copyData :: [ElfXX a] -> Memory -> Address -> IO ()
copyData [] _ _ = pure ()
copyData ((ElfRawData{..}):xs) mem addr = do
    storeByteString mem addr edData
    copyData xs mem addr
copyData ((ElfSection{esData = ElfSectionData textData}):xs) mem addr = do
    storeByteString mem addr textData
    copyData xs mem addr
copyData (x:xs) mem addr = copyData xs mem addr

-- Load all loadable segments of an ELF file into memory.
loadElf :: Memory -> Elf -> IO ()
loadElf mem (SELFCLASS32 :&: ElfList elfs) = do
    loadable <- loadableSegments elfs
    -- TODO: Load zero as specified by epAddMemSize
    mapM (\ElfSegment{..} -> copyData epData mem $ toMemAddr epPhysAddr) loadable
    pure ()

main :: IO ()
main = do
    args <- getArgs
    if (length args) /= 1
        then error "Accepting only a single file argument"
        else do
            mem <- mkMemory 32
            elf <- readElf $ head args

            loadElf mem elf
            state <- mkArchState mem

            -- TODO: Extract start address from elf
            executeAll state 0x0
            out <- dumpRegs $ fst state
            putStr out
