module Lib ( parseAndEval ) where

import Lexer
import Parser
import Semantic
import Eval
  
parseAndEval :: String -> Int -> ExpL
parseAndEval s f = finalExp
  where 
    ast          = progToProgS [] $ parse $ alexScanTokens s
    deBruijnAST  = typecheck ast []
    expandedAST  = inlineF [] deBruijnAST f
    --(inlAST, ls) = hardInline expandedAST
    (expS, ls)   = closure expandedAST
    finalExp     = expStoExpL expS ls