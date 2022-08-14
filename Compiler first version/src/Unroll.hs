module Unroll where

import Semantic

inline :: ExpS -> Context -> String -> ExpS -> ExpS
inline (VarS v)             env s ex
  | v == s    = ex
  | otherwise = VarS v 
inline (ZeroS)                env s ex = ZeroS
inline (SucS e)               env s ex = SucS (inline e env s ex) 
inline (AbsS v t e)           env s ex = AbsS v t (inline e ((v, t):env) s ex)
inline (AppS e1 e2)           env s ex = AppS (inline e1 env s ex) (inline e2 env s ex) 
inline (MatchS e1 e2 (v, e3)) env s ex = MatchS 
                                            (inline e1 env s ex) 
                                            (inline e2 env s ex) 
                                            (v, inline e3 ((v, NatS):env) s ex)

finline' :: Context -> String -> ExpS -> ExpS -> Int -> ExpS
finline' env s e e' 0 = e
finline' env s e e' n = finline' env s (inline e env s e') e' (n-1)

finline :: Context -> String -> ExpS -> Int -> ExpS
finline env s e n = finline' env s e e n 

inlineF :: Context -> ProgS -> Int -> ProgS
inlineF env (MainS t e)            _ = MainS t e
inlineF env (DeclS (FunS s t e) p) n = DeclS (FunS s t (finline ((s, t):env) s e n)) (inlineF ((s, t):env) p n)  

