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
toMemAddr addr = addr - memoryStart

readElf :: FilePath -> IO (Elf)
readElf path = do
    bs <- readFileLazy path
    parseElf bs

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

getWordXXS :: Sing a -> WordXX a -> Word32
getWordXXS SELFCLASS32 w = w

getWordXX :: SingI a => WordXX a -> Word32
getWordXX = withSing getWordXXS

-- Copy raw data from ELF to memory at the given absolute address.
copyData :: (IsElfClass a) => [ElfXX a] -> Memory -> IO ()
copyData [] _ = pure ()
copyData ((ElfSection{esData = ElfSectionData textData, ..}):xs) mem = do
    storeByteString mem (toMemAddr $ getWordXX esAddr) textData
    copyData xs mem
copyData (x:xs) mem = copyData xs mem

-- Load all loadable segments of an ELF file into memory.
loadElf :: Memory -> Elf -> IO (Word32)
loadElf mem elf@(classS :&: ElfList elfs) = withElfClass classS $ do
    loadable <- loadableSegments elfs
    -- TODO: Load zero as specified by epAddMemSize
    mapM (\ElfSegment{..} -> copyData epData mem) loadable
    startAddr elf

main :: IO ()
main = do
    args <- getArgs
    if (length args) /= 1
        then error "Accepting only a single file argument"
        else do
            mem <- mkMemory 1024
            elf <- readElf $ head args

            entry <- loadElf mem elf
            state <- mkArchState mem

            putStrLn "\nExecuting all instructions…"
            executeAll state $ toMemAddr entry

            putStrLn "\nDumping register file…"
            out <- dumpRegs $ fst state
            putStr out
