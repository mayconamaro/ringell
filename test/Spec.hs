import Generation ()
import Generator
import Test.QuickCheck

main :: IO ()
main = do
    quickCheck propWellTyped