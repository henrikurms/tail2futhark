import Test.Tasty
import Test.Tasty.Program
import Test.Tasty.Golden
import System.Process

main = defaultMain tests

tests = testGroup "Tests" [crashTests,outputTests]

crashTests = testGroup "Crash Tests" [testProgram "test2.tail" "tail2futhark" ["tests/test2.tail"] Nothing]

outputTests = testGroup "Output Tests" [goldenVsFile "integer test" "tests/integer.fut" "tests/integer_out.fut" integerTest]


integerTest :: IO ()
--integerTest = rawSystem "tail2futhark" ["tests/integer.tail", "-o", ""]
integerTest = system "tail2futhark tests/integer.tail > tests/integer_out.fut" >> return ()
