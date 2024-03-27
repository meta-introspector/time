
{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH

-- Foo data type
data Foo
  = FooIdentifier Name
  | FooVariable Name String
  | FooComposed [Foo]

-- Convert Q Exp to Foo
qToFoo :: Q Exp -> Q Foo
qToFoo qexp = qexp >>= convertExp
  where
    convertExp (VarE name) = return (FooVariable name "")
    convertExp (ConE name) = return (FooIdentifier name)
    convertExp (AppE e1 e2) = do
      foo1 <- qToFoo (return e1)
      foo2 <- qToFoo (return e2)
      return (FooComposed [foo1, foo2])
    convertExp _ = error "Unsupported expression"

-- Convert Foo to String
fooToString :: Foo -> String
fooToString (FooIdentifier name) = show name
fooToString (FooVariable name value) = show name ++ " (" ++ value ++ ")"
fooToString (FooComposed foos) = concatMap fooToString foos

-- Example usage
main :: IO ()
main = do
  let example = [| fooToString |]
  foo <- runQ (qToFoo example)
  putStrLn "Original Q expression:"
  -- putStrLn (pprint example)
  putStrLn "\nFoo conversion:"
  putStrLn (fooToString foo)
