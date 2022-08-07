{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

module Main where

import Utils
import Decoder
import Control.Monad.Catch
import Data.Bits
import Data.Int
import Data.Word
import Data.Elf
import Data.Elf.Headers
import Data.Elf.Constants
import Data.Elf.PrettyPrint (readFileLazy)
import Data.Singletons
import Data.Singletons.Sigma
import qualified Data.ByteString.Lazy as BSL
import System.FilePath
import System.Environment

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

textSection :: Elf -> IO (Maybe BSL.ByteString)
textSection (SELFCLASS32 :&: ElfList elfs) = do
    sec <- elfFindSectionByName elfs ".text"
    return $ case sec of
        ElfSection { esData = ElfSectionData textData } -> Just textData
        _ -> Nothing

decodeInstr :: Word32 -> String
decodeInstr = show . decode

decodeAll :: BSL.ByteString -> String
decodeAll bs = foldr (\bs str -> (decodeInstr $ fstWord bs) ++ str) "" lst
    where
        lst = takeWhile (not . BSL.null) $ iterate (BSL.drop 4) bs

main' :: Elf -> IO ()
main' elf = do
    text <- textSection elf
    case text of
        Just x  -> putStrLn $ decodeAll x
        Nothing -> putStrLn "No text segment"

main :: IO ()
main = do
    args <- getArgs
    if (length args) /= 1
        then error "Accepting only a single file argument"
        else do
            elf <- readElf $ head args
            main' elf
