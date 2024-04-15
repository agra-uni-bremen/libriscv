{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module LibRISCV.Internal.Decoder.Generator where

import Language.Haskell.TH hiding (match)
import LibRISCV.Internal.Decoder.YamlParser
import Data.Yaml
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad (forM)
import qualified Data.Map.Strict as M
import Data.List (find, sortBy, nub)
import Data.Char (toUpper)
import Data.Function (on)
import Data.Bits ((.&.))
import Data.Word (Word32)
import Data.Maybe (isJust, fromJust)
import Data.FileEmbed (makeRelativeToProject)



readAndParse :: MonadIO m => FilePath -> m [(String, InstructionFields)]
readAndParse filePath = M.toList <$> liftIO (decodeFileThrow filePath)

generateConsts :: [(String, InstructionFields)] -> String -> (InstructionFields -> String) -> Q [Dec]
generateConsts entries suffix f = do
    forM entries $ \(instructionName, fields) -> do
        let
            nameD = mkName $ instructionName <> "_" <> suffix
            valE= read $ f fields
        pure $ ValD
            (VarP nameD)
            (NormalB $ LitE $ IntegerL valE)
            []

generateExtType :: [(String, InstructionFields)] -> Q [Dec]
generateExtType entries = do
    let
        byExts = M.fromListWith (<>)
            . map (\(x,y) -> (x, [y]))
            . concatMap (\(inst, fs) ->
                map (, inst) (extension fs)) $ entries
        superType = DataD
            []
            (mkName "InstructionType")
            []
            Nothing
            (NormalC (mkName "InvalidInstruction") [] : map (\ex ->
                NormalC
                    (mkName $ map toUpper ex)
                    [(Bang NoSourceUnpackedness NoSourceStrictness, ConT (mkName $ map toUpper ex))])
                        (M.keys byExts))
            [DerivClause Nothing [ConT ''Eq, ConT ''Show]]
    extTypes <- forM (M.toList byExts) $ \(ext, instructions) -> do
        pure $ DataD
            []
            (mkName $ map toUpper ext)
            []
            Nothing
            (map (\instr -> NormalC (mkName $ map toUpper instr) []) instructions)
            [DerivClause Nothing [ConT ''Eq, ConT ''Show]]

    pure $ extTypes <> [superType]

generateExtDecodeFns :: [(String, InstructionFields)] -> Q [Dec]
generateExtDecodeFns entries = do
    let 
        byExts = M.fromListWith (<>)
            . map (\(x,y) -> (x, [y]))
            . concatMap (\(inst, fs) ->
                map (, inst) (extension fs)) $ entries
        
    sigs <- forM (M.toList byExts) $ \(ex, _) -> 
        pure $ 
            SigD 
                (mkName $ "decode_" <> map toUpper ex) 
                (AppT 
                    (AppT 
                        ArrowT 
                        (ConT ''Word32)) 
                    (AppT 
                        (ConT ''Maybe)
                        (ConT $ mkName $ map toUpper ex))
                )
    defs <- forM (M.toList byExts) $ \(ex, instrs) -> 
        pure $ FunD 
            (mkName $ "decode_" <> map toUpper ex)
            [Clause 
                [VarP (mkName "w")] 
                (GuardedB $
                    map (\instr -> 
                            (NormalG $ 
                                InfixE 
                                    (Just (InfixE 
                                        (Just (VarE $ mkName "w")) 
                                        (VarE '(.&.)) 
                                        (Just (VarE $ mkName $ instr <> "_mask")))) 
                                    (VarE '(==)) 
                                    (Just (VarE $ mkName $ instr <> "_match")) 
                            , AppE (ConE 'Just) $ ConE $ mkName $ map toUpper instr)
                        ) instrs <>
                        [(NormalG (ConE 'True), ConE 'Nothing)])
                []
            ]
    pure $ sigs <> defs    
    

generateDecodeFn :: [(String, InstructionFields)] -> Q [Dec]
generateDecodeFn entries = do
    let 
        exts = map (map toUpper) 
            . nub . concatMap (extension . snd) $ entries
        sig = 
            SigD 
                (mkName "decode")
                (AppT
                    (AppT 
                        ArrowT
                        (ConT ''Word32))
                    (ConT $ mkName "InstructionType")
                )
        def = 
            FunD 
                (mkName "decode")
                [Clause 
                    [VarP $ mkName "w"]
                    (GuardedB $ 
                        map (\ex -> 
                                (NormalG $
                                    AppE (VarE 'isJust)
                                    (AppE 
                                        (VarE $ mkName $ "decode_" <> ex) 
                                        (VarE $ mkName "w"))
                                , AppE 
                                    (ConE $ mkName ex)
                                    (AppE 
                                        (VarE 'fromJust)
                                        (AppE 
                                            (VarE $ mkName $ "decode_" <> ex)
                                            (VarE $ mkName "w")))
                                )
                            ) exts <> [(NormalG (ConE 'True), ConE $ mkName "InvalidInstruction")]
                    )     
                    []
                ]
    pure [def, sig]


generateMasks :: [(String, InstructionFields)] -> Q [Dec]
generateMasks entries = generateConsts entries "mask" mask

generateMatches :: [(String, InstructionFields)] -> Q [Dec]
generateMatches entries = generateConsts entries "match" match

generateAll :: [[(String, InstructionFields)] -> Q [Dec]] -> FilePath -> Q [Dec]
generateAll seqs path = do
    entries <- liftIO $ readAndParse path
    concat <$> mapM (\f -> f entries) seqs

generateDefaultDecoder :: Q [Dec]
generateDefaultDecoder = makeRelativeToProject "data/instr_dict.yaml"  >>= generateAll 
    [ generateMasks
    , generateMatches
    , generateExtType
    , generateExtDecodeFns
    , generateDecodeFn]
