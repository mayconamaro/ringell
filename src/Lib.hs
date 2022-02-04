module Lib ( parseAndEval ) where

import Lexer
import Parser
import Semantic
import Unroll
import EvalL
  
parseAndEval :: String -> Int -> String
parseAndEval s f = prettyPrint . quote $ eval finalExp []
  where 
    ast          = progToProgS [] $ parse $ alexScanTokens s
    tyAST        = (\_ -> ast) (typecheck ast [])
    expandedAST  = inlineF [] tyAST f
    finalExp     = progStoExpL expandedAST