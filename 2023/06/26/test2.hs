{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

listMembers :: Q [Con]
listMembers = do
  info <- reify (mkName "Language.Haskell.TH.Syntax")
  case info of
    TyConI (DataD _ _ _ _ constructors _) -> return constructors
    _ -> fail "Invalid type"

main :: IO ()
main = do
  members <- runQ listMembers
  putStrLn "Members:"
  mapM_ (putStrLn . pprint) members
