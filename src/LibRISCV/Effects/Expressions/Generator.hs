-- This module generates smart constructors for the LibRISCV expressions
-- language through template-haskell. These constructors are syntactic sugar
-- intended to ease usage of the expression language.
module LibRISCV.Effects.Expressions.Generator where

import Data.Char (toLower)
import Language.Haskell.TH
import LibRISCV.Effects.Expressions.Type

generateImmediate :: String -> Q Dec
generateImmediate operator = do
  let name = mkName $ (map toLower operator) ++ "Imm"

  let onName   = mkName "on"
  let consName = mkName operator
  let immName  = mkName "FromImm"

  let body = AppE (AppE (VarE onName) (ConE consName)) (ConE immName)
  return $ FunD name [Clause [] (NormalB body) []]

generateImmediates :: Q [Dec]
generateImmediates =
  mapM generateImmediate [
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
