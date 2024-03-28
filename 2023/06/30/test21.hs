{-# LANGUAGE TemplateHaskell #-}
import Language.Haskell.TH



       
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

applyReification :: ExpQ -> Q ()
applyReification exp = do
  exp' <- unTypeExp exp
  info <- reify exp'
  runIO $ print info
  case info of
    VarI _ t _ -> applyReification (return t)
    _ -> return ()

main :: IO ()
main = do
  putStrLn "Initial expression: x = 1 + 1"
  applyReification [|x|]
