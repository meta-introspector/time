{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH

-- Foo data type
data Foo
  = FooIdentifier Name
  | FooVariable Name String
  | FooComposed [Foo]

-- Convert Q Exp to Foo
qToFoo :: Q Exp -> Foo
qToFoo qexp = case unTypeQ qexp of
  VarE name -> FooVariable name ""
  ConE name -> FooIdentifier name
  AppE e1 e2 -> FooComposed [qToFoo (return e1), qToFoo (return e2)]
  _ -> error "Unsupported expression"

-- Convert Foo to String
fooToString :: Foo -> String
fooToString (FooIdentifier name) = show name
fooToString (FooVariable name value) = show name ++ " (" ++ value ++ ")"
fooToString (FooComposed foos) = concatMap fooToString foos

-- Example usage
main :: IO ()
main = do
  let example = [| (x + 1) * y |]
      foo = qToFoo example
  putStrLn "Original Q expression:"
  putStrLn (pprint example)
  putStrLn "\nFoo conversion:"
  putStrLn (fooToString foo)
