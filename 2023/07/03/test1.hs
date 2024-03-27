{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH

-- extractFunctionNames :: [Dec] -> [Name]
-- extractFunctionNames decs = [name | FunD name _ <- decs]
import Foo

main :: IO ()




main = do
  putStrLn $(stringE . show =<< (Foo.foo (reify ''Dec)) )
