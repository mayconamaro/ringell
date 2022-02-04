module EvalR where

import Semantic

{-- High Order Abstract Syntax by McBride --}

data Name = Local Int
          | Quote Int
          deriving (Eq, Show)

data ExpR = Free Name 
          | Bound Int
          | AppR ExpR ExpR
          | ZeroR
          | SucR ExpR
          | AbsR ExpR
          | MatchR ExpR ExpR ExpR
          deriving (Eq, Show)

data VNum = VZero
          | VSuc VNum

data Value = VAbs (Value -> Value)
           | VNumber VNum
           | VNeutral Neutral

data Neutral = NFree Name
             | NApp Neutral Value

type Env = [Value]

evalR :: ExpR -> Env -> Value
evalR (Free x)          env = vfree x
evalR (Bound i)         env = if i >= length env 
                                then error $ "Tried to access index " ++ show i ++ " in " ++ show (map quoteR env) 
                                else env !! i
evalR (AppR e1 e2)      env = vapp (evalR e1 env) (evalR e2 env)
evalR (AbsR e)          env = VAbs (\x -> evalR e (x:env))
evalR (ZeroR)           env = VNumber VZero
evalR (SucR e)          env = vsuc (evalR e env)
evalR (MatchR e1 e2 e3) env = vmatch (evalR e1 env) env e2 e3

vmatch :: Value -> Env -> ExpR -> ExpR -> Value
vmatch (VNumber VZero)     env e1 _ = evalR e1 env
vmatch (VNumber (VSuc x))  env _ e2 = evalR e2 ((VNumber x):env)
vmatch e                   _   _ _  = error $ "tried to match " ++ show (quoteR e)

vsuc :: Value -> Value
vsuc (VNumber n) = VNumber (VSuc n)

vapp :: Value -> Value -> Value 
vapp (VAbs f)     v       = f v
vapp (VNeutral n) v       = VNeutral (NApp n v)

vfree :: Name -> Value
vfree n = VNeutral (NFree n)

quoteR :: Value -> ExpR
quoteR = quote' 0

quote' :: Int -> Value -> ExpR
quote' i (VAbs f)           = AbsR (quote' (i + 1) (f (vfree (Quote i))))
quote' i (VNumber VZero)    = ZeroR
quote' i (VNumber (VSuc n)) = SucR (quote' i (VNumber n)) 
quote' i (VNeutral n)       = neutralQuote i n

neutralQuote :: Int -> Neutral -> ExpR
neutralQuote i (NFree x)  = boundfree i x
neutralQuote i (NApp n v) = AppR (neutralQuote i n) (quote' i v) 

boundfree :: Int -> Name -> ExpR
boundfree i (Quote k) = Bound (i - k - 1)
boundfree i  x        = Free x  

expStoExpR :: ExpS -> Context -> ExpR
expStoExpR (VarS v)               ctx = Bound (find v ctx)
expStoExpR (ZeroS)                ctx = ZeroR
expStoExpR (SucS e)               ctx = SucR (expStoExpR e ctx )
expStoExpR (AbsS v t e)           ctx = AbsR (expStoExpR e ((v, t):ctx))
expStoExpR (AppS e1 e2)           ctx = AppR (expStoExpR e1 ctx) (expStoExpR e2 ctx)
expStoExpR (MatchS e1 e2 (v, e3)) ctx = MatchR (expStoExpR e1 ctx) 
                                           (expStoExpR e2 ctx) (expStoExpR e3 ((v, NatS):ctx))

find :: String -> Context -> Int
find s ((s', t):xs)
  | s == s'   = 0
  | otherwise = 1 + find s xs

combY :: ExpR
combY = AbsR (AppR 
                (AbsR (AppR (Bound 1) (AppR (Bound 0) (Bound 0)))) 
                (AbsR (AppR (Bound 1) (AppR (Bound 0) (Bound 0)))))

progStoExpR' :: ProgS -> Context -> ExpR
progStoExpR' (MainS t e)            ctx = expStoExpR e ctx
progStoExpR' (DeclS (FunS s t e) p) ctx = AppR (AbsR (progStoExpR' p ((s, t):ctx))) 
                                               (AppR combY (AbsR (expStoExpR e ((s, t):ctx))))

progStoExpR :: ProgS -> ExpR
progStoExpR p = progStoExpR' p []

prettyPrintR :: ExpR -> String
prettyPrintR (ZeroR)  = "0"
prettyPrintR (SucR e) = show (1 + calc e)
  where 
    calc (ZeroR)  = 0
    calc (SucR e) = 1 + calc e
prettyPrintR e     = show e