import Generation ()
import Generator
import Test.QuickCheck
import Test.HUnit
import Parsing (allUnitTests)

main :: IO ()
main = do
    runTestTT allUnitTests
    quickCheck (withMaxSuccess 100 propWellTyped)
    quickCheck (withMaxSuccess 100 propSemPreservation)