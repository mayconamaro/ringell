import Generation ()
import Generator
import Test.QuickCheck

main :: IO ()
main = do
    quickCheck (withMaxSuccess 1000 propWellTyped)
    quickCheck (withMaxSuccess 1000 propEval)