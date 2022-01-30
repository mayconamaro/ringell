module Lib ( someFunc ) where

import System.Environment (getArgs)
import Lexer
import Parser
import Semantic

someFunc :: IO ()
someFunc = do
  args <- getArgs
  s <- readFile (head args)
  case typecheck ((progToProgS . parse . alexScanTokens) s) [] of
    () -> return ()
  
