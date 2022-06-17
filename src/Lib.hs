module Lib ( parseAndEval, parseAndEvalR ) where

import Lexer
import Parser
import Semantic
import Unroll
import EvalL
import qualified EvalPierceR as P

parseAndEvalR :: String -> String
parseAndEvalR s = (P.prettyPrintR $ P.eval finalExp)
  where 
    ast          = progToProgS [] $ parse $ alexScanTokens s
    tyASTStatus  = typecheck ast []
    tyAST        = case tyASTStatus of 
      Ok     -> ast
      Fail x -> error x 
    finalExp     = P.progStoExpR tyAST
  
parseAndEval :: String -> Int -> String
parseAndEval s f = prettyPrint $ eval finalExp
  where 
    ast          = progToProgS [] $ parse $ alexScanTokens s
    tyASTStatus  = typecheck ast []
    tyAST        = case tyASTStatus of 
      Ok     -> ast
      Fail x -> error x 
    expandedAST  = inlineF [] tyAST f
    finalExp     = progStoExpL expandedAST