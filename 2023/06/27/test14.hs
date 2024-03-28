{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH
import Data.List (intercalate)

-- Define the AST data type
data AST
  = Identity String
  | KeyValue [(String, AST)]
  | Object [AST]

-- Define a helper function to convert a list of key-value pairs into AST
toKeyValue :: [(String, AST)] -> AST
toKeyValue pairs = KeyValue pairs

-- Define the Unparse class
class Unparse a where
  unparse :: a -> AST

-- Implement Unparse for Name
instance Unparse Name where
  unparse = Identity . show

-- Implement Unparse for Type
instance Unparse Type where
  unparse (ConT name) = toKeyValue [("Typ33e", unparse name)]
  unparse otherType = Identity $ show otherType

-- Implement Unparse for Dec
instance Unparse Dec where
  unparse = Identity . show

-- Implement Unparse for Exp
instance Unparse Exp where
  unparse = Identity . show

-- Function to print the AST as a string
printAST :: AST -> String
printAST (Identity value) = value
printAST (KeyValue pairs) = "{" ++ intercalate ", " (map (\(key, val) -> key ++ ": " ++ printAST val) pairs) ++ "}"
printAST (Object nodes) = "[" ++ intercalate ", " (map printAST nodes) ++ "]"

-- Example usage
main :: IO ()
main = do
  let exampleName = mkName "example"
      exampleType = ConT ''Int
      exampleDec = ValD (VarP exampleName) (NormalB (LitE (IntegerL 42))) []
      exampleExp = AppE (VarE exampleName) (LitE (IntegerL 10))

  let astName = unparse exampleName
      astType = unparse exampleType
      astDec = unparse exampleDec
      astExp = unparse exampleExp

  putStrLn $ printAST astName
  putStrLn $ printAST astType
  putStrLn $ printAST astDec
  putStrLn $ printAST astExp
