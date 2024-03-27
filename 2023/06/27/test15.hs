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

-- Get all the function names in the current module
getFunctionNames :: Q [Name]
getFunctionNames = do
  info <- reify ''Unparse -- You can replace 'Unparse' with any other type or value in your module
  case info of
    ClassI _ decs -> return [name | FunD name _ <- decs]
    _ -> return []

-- Example usage
main :: IO ()
main = do
  functionNames <- runQ getFunctionNames
  putStrLn "Functions in the current module:"
  mapM_ (putStrLn . unparse) functionNames
