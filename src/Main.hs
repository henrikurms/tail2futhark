module Main where

import System.IO
import System.Environment
import APLAcc.TAIL.Parser (parseFile)
import Tail2Futhark.Futhark.Pretty (prettyPrint)
import Tail2Futhark.Compile (compile)

main :: IO ()
main = do
  args <- getArgs
  program <- case args of [] -> parseFile stdin "stdin"
                          (f : _) -> withFile f ReadMode $ flip parseFile f

  putStrLn . prettyPrint . compile $ program


