module Main where

import System.Environment (getArgs)
import Lib

main :: IO ()
main = do
    args <- getArgs
    case args of 
        []     -> putStrLn "Usage: ringell [FILE] [OPTION]\nWhere OPTION can be empty or a nonnegative integer to act as fuel."
        [p]    -> readFile p >>= \s -> putStrLn $ parseAndEvalR s
        [p, f] -> if (read f < 0) 
                      then putStrLn "Fuel must be nonnegative" 
                      else readFile p >>= \s -> putStrLn $ parseAndEval s (read f)  
