
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Module(Module), reifyModule)

-- modString :: ModName -> String
-- data Exp
-- ...
--  | DoE (Maybe ModName) [Stmt]
--  | MDoE (Maybe ModName) [Stmt]
-- type ModName :: *
-- newtype ModName = ModName String
-- type Module :: *
-- data Module = Module PkgName ModName
-- type ModuleInfo :: *
-- data ModuleInfo = ModuleInfo [Module]
-- type Name :: *
-- data Name = Name OccName NameFlavour


-- Assuming you have a `Module` object named `myModule`
getModuleName :: Module -> String
getModuleName (Module _ (ModuleName modName)) = modName

main :: IO ()
main = do
  moduleName <- runQ $ do
    mod <- thisModule
    reifyMod <- reifyModule mod
    return $ nameModule reifyMod
  putStrLn moduleName
