module Main where

import System.IO (IOMode(ReadMode), withFile, stdin)
import System.Environment
import APLAcc.TAIL.Parser (parseFile)
import APLAcc.Conversion (convertProgram)
import APLAcc.SimpleAcc.ToHaskell (toHs, OutputOpts(..), defaultOpts)

main :: IO ()
main =
  do args <- getArgs
     case parseArgs args defaultOpts of
       (["-"], opts) -> compileFile "stdin" opts stdin
       ([f],   opts) -> withFile f ReadMode (compileFile f opts)
       _             -> putStrLn "usage: aplacc [-c|--cuda] <file>"
  where parseArgs ("-c" : rest) opts     = parseArgs rest (opts { toCUDA = True })
        parseArgs ("--cuda" : rest) opts = parseArgs rest (opts { toCUDA = True })
        parseArgs (x : rest) opts        = let (as, opts') = parseArgs rest opts
                                           in (x:as, opts')
        parseArgs [] opts = ([], opts)

        compileFile file opts h = parseFile h file >>= putStrLn . toHs opts . convertProgram

