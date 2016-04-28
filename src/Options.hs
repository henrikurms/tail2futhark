module Options where

import System.Console.GetOpt

-- Option record --
data Options = Options { outputFile :: Maybe String
                       , includeLibs :: Bool
                       , floatAsSingle :: Bool
                       }
             deriving Show

defaultOptions :: Options
defaultOptions = Options {outputFile = Nothing,
                          includeLibs = True,
                          floatAsSingle = False
                         }

-- option description --
options :: [OptDescr (Options -> Options)]
options = [Option ['o'] [] (ReqArg (\arg opt -> opt { outputFile = Just arg }) "FILE") "output FILE",
           Option [] ["no-include-lib-funs"] (NoArg $ \opt -> opt {includeLibs = False }) "include lib functions",
           Option [] ["float-as-single"] (NoArg $ \opt -> opt { floatAsSingle = True })
           "Compile floating-point numbers as single precision."]
