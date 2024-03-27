{-# LANGUAGE TemplateHaskell #-}

module Main where

import Language.Haskell.TH

-- Helper function to list the names of functions in the current module
listFunctionNames :: Q [Name]
listFunctionNames = do
  info <- reify ''Main  -- Replace 'Main' with the actual module name
  case info of
    TyConI (DataD _ _ _ _ decs _) -> return $ extractFunctionNames decs
    _                            -> fail "Invalid module"

-- Extracts the names of functions from a list of declarations
extractFunctionNames :: [Dec] -> [Name]
extractFunctionNames = foldr extract [] where
  extract (FunD name _) names = name : names
  extract _ names             = names

-- Print the names of functions in the current module
main :: IO ()
main = do
  functionNames <- runQ listFunctionNames
  putStrLn "Functions in the current module:"
  mapM_ print functionNames
