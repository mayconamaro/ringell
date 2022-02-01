module Main where

import System.Environment (getArgs)
import Lib

main :: IO ()
main = do
    args <- getArgs
    case args of 
        []     -> putStrLn "I need a .rl program and a fuel value"
        [_]    -> putStrLn "I need a .rl program and a fuel value"
        [p, f] -> if (read f < 0) 
                      then putStrLn "Fuel must be nonnegative" 
                      else readFile p >>= \s -> print $ parseAndEval s (read f)  
