module Main where

import Control.Monad
import System.IO
import System.Exit
import System.Console.GetOpt
import System.Environment
import System.FilePath
import Tail2Futhark.TAIL.Parser (parseFile)
import Tail2Futhark.TAIL.AST (Program)
import Tail2Futhark.Futhark.Pretty (pretty)
import Tail2Futhark.Compile (compile)
import Options

main :: IO ()
main = do
  cmdargs <- getArgs
  let (opts,args,errors) = runArgs cmdargs

  checkErrors errors
  program <- run (library opts) args
  case outputFile opts of
    Nothing -> putStrLn . pretty . compile opts $ program
    Just  f -> withFile f WriteMode (\h -> hPutStr h $ pretty . compile opts $ program)


checkErrors :: [String] -> IO ()
checkErrors [] = return ()
checkErrors errors = hPutStrLn stderr (concat errors ++ usageInfo "Usage: tail2futhark [options] FILE" options) >> exitFailure

-- nonargs list of functions typed : Options -> Options
runArgs :: [String] -> (Options,[String],[String])
runArgs cmdargs = (opts,args,errors)
  where
  (nonargs,args,errors) = getOpt Permute options cmdargs
  opts = foldl (.) id nonargs defaultOptions -- MAGIC!!!! - our ooption record

run :: Bool -> [String] -> IO [(String,Program)]
run True fs = forM fs $ \f -> do
  prog <- withFile f ReadMode (flip parseFile f)
  return (removeDirectory $ dropExtension f, prog)
run False [f] = do
  prog <- withFile f ReadMode $ flip parseFile f
  return [("main", prog)]
run False _ = do hPutStrLn stderr (usageInfo "Usage: tail2futhark [options] FILE" options)
                 exitFailure
