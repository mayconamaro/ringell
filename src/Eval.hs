module Eval where

import Semantic

type Environment = [(String, ExpS)]

lookupEnV :: String -> Environment -> (ExpS, Int)
lookupEnV s e = lookupEnV' s e 0 

lookupEnV' :: String -> Environment -> Int -> (ExpS, Int)
lookupEnV' _   []            _ = error "bug in compiler"
lookupEnV' s ((v, e) : env)  n 
  | s == v    = (e, n)
  | otherwise = lookupEnV' s env (n+1)

recalculateIndices :: ExpS -> Context -> ExpS
recalculateIndices (VarS v i)               env = VarS v (snd $ lookupV env v)
recalculateIndices (ZeroS)                  env = ZeroS
recalculateIndices (SucS e)                 env = SucS (recalculateIndices e env)
recalculateIndices (AbsS v t e)             env = AbsS v t (recalculateIndices e ((v, t):env))
recalculateIndices (AppS e1 e2)             env = AppS (recalculateIndices e1 env) (recalculateIndices e2 env)
recalculateIndices (MatchS e1 e2 (v, e3))   env = MatchS 
                                                      (recalculateIndices e1 env) 
                                                      (recalculateIndices e2 env) 
                                                      (v, (recalculateIndices e3 ((v, NatS):env)))

inline :: ExpS -> Context -> String -> ExpS -> ExpS
inline (VarS v i)             env s ex
  | v == s    = recalculateIndices ex env
  | otherwise = VarS v i 
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

hardInline' :: Environment -> Context -> ProgS -> (ExpS, [String])
hardInline' []            []  (MainS t e)            = (e, [])
hardInline' ((fn, fe):fs) ctx (MainS t e)            = (inline e ctx fn fe, map fst ((fn, fe):fs))  
hardInline' []            []  (DeclS (FunS s t e) p) = hardInline' ((s, e):[]) ((s, t):[]) p
hardInline' ((fn,fe):fs)  ctx (DeclS (FunS s t e) p) = hardInline' ((s, ie):((fn, fe):fs)) ctx p 
    where ie = inline e ctx fn fe

hardInline :: ProgS -> (ExpS, [String])
hardInline p = hardInline' [] [] p 

closure :: ProgS -> (ExpS, [String])
closure (MainS t e)            = (e, [])
closure (DeclS (FunS s t e) p) = (AppS (AbsS s t (fst $ closure p)) e, (s : (snd $ closure p)))

-- Representation for Evaluation
data ExpL 
    = Error
    | VarL Int
    | ZeroL  
    | SucL ExpL
    | AbsL TypeS ExpL
    | AppL ExpL ExpL
    | MatchL ExpL ExpL ExpL
    deriving (Eq, Show)

expStoExpL :: ExpS -> [String] -> ExpL
expStoExpL (VarS v i)             s = if elem v s then Error else VarL i
expStoExpL (ZeroS)                s = ZeroL
expStoExpL (SucS e)               s = SucL (expStoExpL e s)
expStoExpL (AbsS v t e)           s = AbsL t (expStoExpL e s)
expStoExpL (AppS e1 e2)           s = AppL (expStoExpL e1 s) (expStoExpL e2 s)
expStoExpL (MatchS e1 e2 (v, e3)) s = MatchL (expStoExpL e1 s) (expStoExpL e2 s) (expStoExpL e3 s)

-- data Value = Number Int | Function ExpL | VError

-- instance Show Value where
--     show (Number n)   = show n
--     show (Function e) = show e
--     show (VError)     = "out of fuel"

-- type LEnvironment = [ExpL]

-- isValue :: ExpL -> Bool
-- isValue ZeroL        = True
-- isValue (SucL e)     = isValue e
-- isValue (AbsL t e)   = isValue e
-- isValue _            = False 

-- eval :: ExpL -> LEnvironment -> ExpL
-- eval (Error)           env = Error 
-- eval (VarL i)          env = env !! i
-- eval (ZeroL)           env = ZeroL
-- eval (SucL e)          env =
--   case eval e env of
--       Error -> Error
--       exp   -> SucL exp
-- eval (AbsL t e)        env = AbsL t e
-- eval (AppL e1 e2)      env =
--   case eval e2 env of
--     Error -> Error
--     exp   -> case eval e1 (exp:env) of 
--                       Error    -> Error
--                       AbsL t e -> eval e (exp:env)
--                       _        -> error "Compiler has a bug on app"
-- eval (MatchL e1 e2 e3) env =
--   case eval e1 env of
--     Error  -> Error
--     ZeroL  -> eval e2 env
--     SucL n -> eval e3 (n:env)






-- expStoExpL' :: ExpS -> ExpL
-- expStoExpL' (VarS v)               = VarL v
-- expStoExpL' (ZeroS)                = ZeroL
-- expStoExpL' (SucS e)               = SucL (expStoExpL' e)
-- expStoExpL' (AbsS v t e)           = AbsL v t (expStoExpL' e)
-- expStoExpL' (AppS e1 e2)           = AppL (expStoExpL' e1) (expStoExpL' e2)
-- expStoExpL' (MatchS e1 e2 (v, e3)) = MatchL (expStoExpL' e1) (expStoExpL' e2) (v, expStoExpL' e3)

-- progStoExpL :: ProgS -> ExpL
-- progStoExpL (MainS t e)            = expStoExpL' e
-- progStoExpL (DeclS (FunS s t e) p) = AppL (AbsL s t (progStoExpL p)) (expStoExpL e s)

-- eraseNames