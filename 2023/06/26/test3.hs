{-# LANGUAGE TemplateHaskell #-}

module THExample where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

listMembers :: Q [Con]
listMembers = do
  info <- reify (mkName "Language.Haskell.TH.Syntax")
  case info of
    TyConI (DataD _ _ _ _ constructors _) -> return constructors
    _ -> fail "Invalid type"
import Language.Haskell.TH

getMembers :: Q [Dec] -> Q [Dec]
getMembers qDecs = qDecs

doTheThing :: Q [Dec] -> Q [Dec]
doTheThing = getMembers

main :: IO ()
main = do
  let constructors = $(doTheThing [d| data Color = Red |])
  mapM_ (putStrLn . pprint) constructors
