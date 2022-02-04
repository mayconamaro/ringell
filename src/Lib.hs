module Lib ( parseAndEval, parseAndEvalR ) where

import Lexer
import Parser
import Semantic
import Unroll
import EvalL
import EvalR

parseAndEvalR :: String -> String
parseAndEvalR s = prettyPrintR . quoteR $ evalR finalExp []
  where 
    ast          = progToProgS [] $ parse $ alexScanTokens s
    tyAST        = (\_ -> ast) (typecheck ast [])
    finalExp     = progStoExpR tyAST
  
parseAndEval :: String -> Int -> String
parseAndEval s f = prettyPrint . quote $ eval finalExp []
  where 
    ast          = progToProgS [] $ parse $ alexScanTokens s
    tyAST        = (\_ -> ast) (typecheck ast [])
    expandedAST  = inlineF [] tyAST f
    finalExp     = progStoExpL expandedAST