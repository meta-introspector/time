module Foo where

{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH
{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

getObjectNames :: Q Info -> Q Exp
getObjectNames q = do
  names <- q >>= getNames
  listE $ map (litE . stringL) names

getNames :: Info -> Q [String]
getNames (VarI name _ _) = return [nameBase name]
getNames (ClassOpI name  _ _) = return [nameBase name]
getNames (DataConI name _ _ ) = return [nameBase name]
getNames (TyConI (DataD _ _ _ _ constructors _)) = do
  constructorNames <- mapM extractConstructorNames constructors
  return $ concat constructorNames
getNames (TyConI (NewtypeD _ _ _ _ constructor _)) = do
  constructorNames <- extractConstructorNames constructor
  return constructorNames
getNames _ = throwError "Invalid declaration"

extractConstructorNames :: Con -> Q [String]
extractConstructorNames (NormalC name _) = return [nameBase name]
extractConstructorNames (RecC name _) = return [nameBase name]
extractConstructorNames (InfixC _ name _) = return [nameBase name]
extractConstructorNames _ = throwError "Invalid constructor"

throwError :: String -> Q a
throwError err = fail $ "Error: " ++ err

foo = getObjectNames
