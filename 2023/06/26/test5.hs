{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH

getMembers :: Q [Dec] -> Q [Dec]
getMembers qDecs = qDecs
listMembers :: Q a -> Q [Con]
listMembers continuation = do
  info <- reify (mkName "Language.Haskell.TH.Syntax"
)
  case info of
    TyConI (DataD _ _ _ _ constructors _) -> continu
ation >> return constructors
    _ -> fail "Invalid type"
main :: IO ()
main = do
  let constructors = listMembers
  mapM_ (putStrLn . pprint) constructors
