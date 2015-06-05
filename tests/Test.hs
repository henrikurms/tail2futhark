import Test.Tasty
import Test.Tasty.Providers
--import Test.Tasty.Program
import Test.Tasty.Golden
import Test.Tasty.Golden.Manage as G
import System.Process
import System.FilePath.Posix

main = do
    files <- findByExtension [".tail"] "tests/basic_tests"
    let tests = testGroup "Tests"$ map (\f -> goldenVsFile f (replaceExtension f "ok") (replaceExtension f "out") (makeTest f)) files
    G.defaultMain tests

makeTest :: FilePath -> IO ()
makeTest file = rawSystem "tail2futhark" [file,"-o",replaceExtension file "fut"] >>
                system ("futhark -i " ++ replaceExtension file "fut" ++ " < /dev/null > " ++ replaceExtension file "out") >> return ()


--tests = testGroup "Tests" [crashTests,outputTests]
--
--crashTests = testGroup "Crash Tests" [testProgram "test2.tail" "tail2futhark" ["tests/test2.tail"] Nothing]
--
--outputTests = testGroup "Output Tests" [goldenVsFile "integer test" "tests/integer.fut" "tests/integer_out.fut" integerTest]
--
--integerTest :: IO ()
--integerTest = rawSystem "tail2futhark" ["tests/integer.tail", "-o", "tests/integer_out.fut"] >> return ()
