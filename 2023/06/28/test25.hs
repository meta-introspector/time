{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH

-- Foo data type
data Foo
  = FooIdentifier String
  | FooVariable String String
  | FooComposed [Foo]
  | FooList Foo [Foo ]
  | FooFunction String [Foo]
  deriving Show
-- Convert Q Exp to Foo

--listToQ :: [Foo] -> uuQ Foo
--listToQ = foldr (\x acc -> [| $x >>= $acc |]) [| return |]

--listToQ :: [Q Exp] -> Q Foo
--listToQ = foldr (\x acc -> x >>= acc) (return $ FooComposed [])
listToQ :: [Q Exp] -> Q Foo
--listToQ = foldr (\x acc -> x >>= \expr -> acc >>= \foo -> return (FooComposed [expr] : foo)) (return (FooComposed []))

--listToQ = foldr (\x acc -> x >>= \expr -> acc >>= \foo -> return (FooComposed (expr : foo))) (return (FooComposed []))

exprToFoo :: Exp -> Q Foou
exprToFoo expr = return (FooIdentifier (show expr))



listToQ = foldr (\x acc -> x >>= \expr -> acc >>= \foo -> return FooList expr foo) (return (FooComposed []))



listToQ2 = foldr (\x acc -> do
  expr <- x
  FooComposed foo <- acc
  convertedExpr <- exprToFoo expr
  return (FooComposed (convertedExpr : foo))
  ) (return (FooComposed []))


convertDecsToFoo :: [Dec] -> Q Foo
convertDecsToFoo decs = do
  fooDecs <- mapM convertDec decs >>= listToQ
  return (FooComposed (FooIdentifier "LetE" : fooDecs))

convertDec :: Dec -> Q Foo
convertDec (ValD pat rhs decs) = do
  fooPat <- return (FooIdentifier "TODOPats")
  fooRhs <- qToFoo (return rhs)
  fooDecs <- mapM convertDec decs
  return (FooComposed [FooIdentifier "ValD", fooPat, fooRhs, FooComposed fooDecs])

-- Usage:
--fooDecs <- convertDecsToFoo decs
--fooBody <- qToFoo (return body)
--return (FooComposed (FooIdentifier (nameBase (mkName "LetE")) : fooDecs ++ [fooBody]))

qToFoo :: Q Exp -> Q Foo
qToFoo qexp = qexp >>= convertExp
  where
    convertExp (VarE name) = return (FooVariable (nameBase name) "")
    convertExp (ConE name) = return (FooIdentifier (nameBase name))
    --convertExp (LamE
    convertExp (LamE pats body) = do
      --fooPats <- mapM qToFoo (map return pats)
      fooPats <- return [(FooIdentifier "TODOPats")]
      fooBody <- qToFoo (return body)
      return (FooComposed (fooPats ++ [fooBody]))
      
    convertExp (CaseE scrut matches) = do
      fooScrut <- qToFoo (return scrut)
      --fooMatches <- mapM convertMatch matches
      fooMatches <- return [(FooIdentifier "TODOMatches")]
      return (FooComposed (fooScrut : fooMatches))
      where
        convertMatch (match) = do
          return [(FooIdentifier "TODOMatch")]
        convertMatch (pat, body) = do
          fooPat <- qToFoo (return pat)
          fooBody <- qToFoo (return body)
          return [FooComposed [fooPat, fooBody]]
                          
    convertExp (AppE e1 e2) = do
      foo1 <- qToFoo (return e1)
      foo2 <- qToFoo (return e2)
      case foo1 of
        FooIdentifier funcName -> return (FooFunction funcName [foo2])
        FooFunction funcName args -> return (FooFunction funcName (args ++ [foo2]))

    --convertExp _ = error ("Unsupported expression")


    --convertExp (VarE name) = return (FooVariable name "")
    convertExp (ConE name) = return (FooIdentifier (nameBase name))
    convertExp (LitE lit) = return (FooComposed [FooIdentifier (nameBase(mkName "LitE")), FooVariable (nameBase(mkName (show lit))) (show lit)])
    convertExp (AppE e1 e2) = do
      foo1 <- qToFoo (return e1)
      foo2 <- qToFoo (return e2)
      return (FooComposed [foo1, foo2])
    convertExp (LamE pats body) = do
      --fooPats <- mapM qToFoo (map return pats)
      fooPats <- return [(FooIdentifier "TODOPats")]
      fooBody <- qToFoo (return body)
      return (FooComposed (fooPats ++ [fooBody]))
    convertExp (TupE exprs) = do
      --fooExprs <- mapM qToFoo (map return exprs)
      fooExprs <- return [(FooIdentifier "TODOexpr")]
--      return (FooComposed (FooIdentifier (mkName "TupE") : fooExprs))
      return (FooIdentifier ("TODO"))
    convertExp (CondE cond trueExpr falseExpr) = do
      fooCond <- qToFoo (return cond)
      fooTrue <- qToFoo (return trueExpr)
      fooFalse <- qToFoo (return falseExpr)
      return (FooComposed [FooIdentifier (nameBase(mkName "CondE")), fooCond, fooTrue, fooFalse])
    convertExp (LetE decs body) = do
      fooDecs <- listToQ <$> mapM convertDec decs
      fooBody <- qToFoo (return body)
      return (FooComposed (FooIdentifier (nameBase(mkName "LetE")) : fooDecs ++ [fooBody]))
        where
          convertDec (ValD pat rhs decs) = do
            --fooPat <- qToFoo (return pat)
            fooPat <-[(FooIdentifier "TODOPats")]
            fooRhs <- qToFoo (return rhs)
            fooDecs <- mapM convertDec decs
            return (FooComposed [FooIdentifier "ValD", fooPat, fooRhs, FooComposed fooDecs])
    convertDec (FunD name clauses) = do
        fooClauses <- mapM convertClause clauses
        return (FooComposed [FooIdentifier "FunD", FooIdentifier name, FooComposed fooClauses])
    convertDec dec = error ("Unsupported declaration: " ++ show dec)
    convertClause (Clause pats body decs) = do
        fooPats <- mapM qToFoo (map return pats)
        fooBody <- qToFoo (return body)
        fooDecs <- mapM convertDec decs
        return (FooComposed (fooPats ++ [fooBody, FooComposed fooDecs]))
--    convertExp (CaseE scrut matches) = do
--        fooScrut <- qToFoo (return scrut)
--        fooMatches <- mapM convertMatch matches
--        return (FooComposed (fooScrut : fooMatches))
--          where
 --           convertMatch (pat, body) = do
  --            fooPat <- qToFoo (return pat)
  --            fooBody <- qToFoo (return body)
  --            return (FooComposed [fooPat, fooBody])
    --convertExp (UnsupportedE expr) = error ("Unsupported expression: " ++ show expr)


-- Convert Foo to String
fooToString :: Foo -> String
fooToString (FooIdentifier name) = show name
fooToString (FooVariable name value) = show name ++ " (" ++ value ++ ")"
fooToString (FooComposed foos) = concatMap fooToString foos
fooToString (FooFunction name args) = show name ++ " " ++ show args
--fooooo = (fooToString)

f1 = 1
f2 = f1 + f1

-- Example usage
main :: IO ()
main = do
--  let example1 = fooToString  (FooIdentifier "id") 
  let example = [| fooToString  (FooIdentifier "id") |]
  foo <- runQ (qToFoo example)
  putStrLn "Original Q expression:"
--  putStrLn (pprint example1)
  putStrLn "\nFoo conversion:"
  putStrLn (fooToString foo)
