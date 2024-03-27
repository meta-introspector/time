{-# LANGUAGE TemplateHaskell #-}

module Main where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (lift)

-- Helper function to list the names of functions in the current module
listFunctionNames :: Q [Name]
listFunctionNames = do
  currentModule <- thisModule
  annotations <- reify currentModule
  return $ extractFunctionNames annotations

-- Extracts the names of functions from a list of annotations
--extractFunctionNames :: [AnnTargetInfo] -> [Name]
extractFunctionNames = foldr extract [] where
  extract names = names

-- Print the names of functions in the current module
main :: IO ()
main = do
  functionNames <- runQ listFunctionNames
  putStrLn "Functions in the current module:"
  mapM_ print functionNames
