-- This module generates smart constructors for the LibRISCV expressions
-- language through template-haskell. These constructors are syntactic sugar
-- intended to ease usage of the expression language.
module LibRISCV.Effects.Expressions.Generator (generateImmediates) where

import Data.Char (toLower)
import Language.Haskell.TH
import LibRISCV.Effects.Expressions.Type

binOps :: [String]
binOps = [
  "Add"
  , "Sub"
  , "Eq"
  , "Slt"
  , "Sge"
  , "Ult"
  , "Uge"
  , "And"
  , "Or"
  , "Xor"
  , "LShl"
  , "LShr"
  , "AShr"
  , "Mul"
  , "UDiv"
  , "SDiv"
  , "URem"
  , "SRem"
  ]

------------------------------------------------------------------------

foldcase :: String -> String
foldcase = map toLower

genImm :: String -> Q Dec
genImm operator = do
  let name = mkName $ foldcase operator

  let consName = mkName operator
  let immName = mkName "FromImm"
  rval <- newName "rval"
  lval <- newName "lval"

  let body = AppE (AppE (ConE consName) (AppE (ConE immName) (VarE lval)))
                  (AppE (ConE immName) (VarE rval))
  return $ FunD name [Clause [VarP lval, VarP rval] (NormalB body) []]

genImmRval :: String -> Q Dec
genImmRval operator = do
  let name = mkName $ (foldcase operator) ++ "Imm"

  let onName   = mkName "on"
  let consName = mkName operator
  let immName  = mkName "FromImm"

  let body = AppE (AppE (VarE onName) (ConE consName)) (ConE immName)
  return $ FunD name [Clause [] (NormalB body) []]

generateImmediates :: Q [Dec]
generateImmediates = do
  l1 <- mapM genImmRval binOps
  l2 <- mapM genImm binOps
  pure (l1 ++ l2)
