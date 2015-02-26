module Main where

import System.IO
import System.Environment
import APLAcc.TAIL.Parser (parseFile)
import APLAcc.TAIL.AST as T -- the TAIL AST
import Futhark.AST as F -- the futhark AST

main :: IO ()
main = do
  args <- getArgs
  program <- case args of [] -> parseFile stdin "stdin"
                          (f : _) -> withFile f ReadMode $ flip parseFile f
  -- putStrLn $ show program
  putStrLn $ show $ compile program


compile :: T.Program -> F.Program
compile e = [(RealT, "main", [], (compileExp e))]

compileExp :: T.Exp -> F.Exp
compileExp (T.Let id t e1 e2) = F.Let (Ident id) (compileExp e1) (compileExp e2)
compileExp rest = F.Var $ show rest -- catch all
