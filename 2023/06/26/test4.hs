import Language.Haskell.TH

getMembers :: Q [Dec] -> Q [Dec]
getMembers qDecs = qDecs

doTheThing :: Q [Dec] -> Q [Dec]
doTheThing = getMembers

main :: IO ()
main = do
  let constructors = $(doTheThing [d| data Color = Red |])
  mapM_ (putStrLn . pprint) constructors
