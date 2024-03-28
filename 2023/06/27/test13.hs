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
  unparse (ConT name) = [("Type", show name)]
  unparse (AppT t1 t2) = [("Type", "AppT")]ff ++ unparse t1 ++ unparse t2
  unparse (ListT) = [("Type", "ListT")]
  -- Add more cases for other types as needed

-- Example usage
main :: IO ()
main = do
  let exampleType = AppT (ConT ''Maybe) (AppT ListT (ConT ''Int))
  
  printKeyValuePairs $ unparse exampleType

printKeyValuePairs :: [(String, String)] -> IO ()
printKeyValuePairs = mapM_ (\(k, v) -> putStrLn $ k ++ ": " ++ v)
