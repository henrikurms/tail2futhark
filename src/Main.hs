module Main where

import System.IO
import System.Exit
import System.Console.GetOpt
import System.Environment
import APLAcc.TAIL.Parser (parseFile)
import Tail2Futhark.Futhark.Pretty (prettyPrint)
import Tail2Futhark.Compile (compile)

main :: IO ()
main = do
  cmdargs <- getArgs
  --program <- case args of [] -> parseFile stdin "stdin"
  --                        (f : _) -> withFile f ReadMode $ flip parseFile f
  let (opts,args,errors) = runArgs cmdargs

  checkErrors errors
  program <- run args
  case outputFile opts of
    Nothing -> putStrLn . prettyPrint . compile $ program
    Just  f -> withFile f WriteMode (\h -> hPutStr h $ prettyPrint . compile $ program)


checkErrors [] = return ()
checkErrors errors = putStrLn (concat errors ++ usageInfo "Usage: tail2futhark [options] FILE" options) >> exitFailure

data Options = Options {outputFile :: Maybe String} deriving Show

defaultOptions = Options {outputFile = Nothing}

options = [Option ['o'] [] (ReqArg (\arg opt -> opt { outputFile = Just arg }) "FILE") "output FILE"]

runArgs :: [String] -> (Options,[String],[String])
runArgs cmdargs = (opts,args,errors)
  where
  (nonargs,args,errors) = getOpt Permute options cmdargs
  opts = foldl (.) id nonargs defaultOptions

run [] = putStrLn (usageInfo "Usage: tail2futhark [options] FILE" options) >> exitFailure
run (f : _) = withFile f ReadMode $ flip parseFile f
