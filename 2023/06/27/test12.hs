
{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH

-- Define the Unparse class
class Unparse a where
  unparse :: a -> String

-- Implement Unparse for Name
instance Unparse Name where
  unparse = show

-- Implement Unparse for Type
instance Unparse Type where
  unparse = show

-- Implement Unparse for Dec
instance Unparse Dec where
  unparse = show

-- Implement Unparse for Exp
instance Unparse Exp where
  unparse = show

-- Example usage
main :: IO ()
main = do
  let exampleName = mkName "example"
      exampleType = ConT ''Int
      exampleDec = ValD (VarP exampleName) (NormalB (LitE (IntegerL 42))) []
      exampleExp = AppE (VarE exampleName) (LitE (IntegerL 10))

  putStrLn $ unparse exampleName
  putStrLn $ unparse exampleType
  putStrLn $ unparse exampleDec
  putStrLn $ unparse exampleExp
