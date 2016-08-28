module Main(main) where

import Control.Monad
import System.Process
import System.FilePath
import System.Exit

import Test.Tasty
import Test.Tasty.Providers
import Test.Tasty.Golden
import Test.Tasty.Golden.Manage as G

main :: IO ()
main = do
    files <- findByExtension [".tail"] "tests/basic_tests"
    let tests = testGroup "Tests"$ map (\f -> goldenVsFile f (replaceExtension f "ok") (replaceExtension f "out") (makeTest f)) files
    G.defaultMain tests

makeTest :: FilePath -> IO ()
makeTest file = do
  ret <- system ("make -s -B -C " ++ dir ++ " " ++ outname ++ "> /dev/null")
  unless (ret == ExitSuccess) $ fail "Test failed."
  where (dir,name) = splitFileName file
        outname = name `replaceExtension` "out"


--tests = testGroup "Tests" [crashTests,outputTests]
--
--crashTests = testGroup "Crash Tests" [testProgram "test2.tail" "tail2futhark" ["tests/test2.tail"] Nothing]
--
--outputTests = testGroup "Output Tests" [goldenVsFile "integer test" "tests/integer.fut" "tests/integer_out.fut" integerTest]
--
--integerTest :: IO ()
--integerTest = rawSystem "tail2futhark" ["tests/integer.tail", "-o", "tests/integer_out.fut"] >> return ()
