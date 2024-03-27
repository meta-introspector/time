{-# LANGUAGE TemplateHaskell #-}



import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Syntax (Module(Module), Q, reifyModule)

getModuleName :: Module -> String
--getModuleName (Module _ modName) = nameBase modName
getModuleName (Module _ modName) = modString modName

main :: IO ()
main = do
  moduleName <- runQ $ do
    mod <- thisModule
    reifyMod <- reifyModule mod
    return $ getModuleName mod
  putStrLn moduleName
