module EvalL where

import Semantic

{-- High Order Abstract Syntax by McBride --}

data Name = Local Int
          | Quote Int
          deriving (Eq, Show)

data ExpL = Free Name 
          | Bound Int
          | AppL ExpL ExpL
          | ZeroL
          | Error
          | SucL ExpL
          | AbsL ExpL
          | MatchL ExpL ExpL ExpL 
          deriving (Eq, Show)

data VNum = VZero
          | VSuc VNum

data Value = VAbs (Value -> Value)
           | VNumber VNum
           | VError
           | VNeutral Neutral

data Neutral = NFree Name
             | NApp Neutral Value

type Env = [Value]

eval :: ExpL -> Env -> Value
eval (Free x)          env = vfree x
eval (Bound i)         env = if i >= length env 
                                then error $ "Tried to access index " ++ show i ++ " in " ++ show (map quote env) 
                                else env !! i
eval (Error)           env = VError
eval (AppL e1 e2)      env = vapp (eval e1 env) (eval e2 env)
eval (AbsL e)          env = VAbs (\x -> eval e (x:env))
eval (ZeroL)           env = VNumber VZero
eval (SucL e)          env = vsuc (eval e env)
eval (MatchL e1 e2 e3) env = vmatch (eval e1 env) env e2 e3

vmatch :: Value -> Env -> ExpL -> ExpL -> Value
vmatch (VNumber VZero)     env e1 _ = eval e1 env
vmatch (VNumber (VSuc x))  env _ e2 = eval e2 ((VNumber x):env)
vmatch (VError)            env _ _  = VError
vmatch e                   _   _ _  = error $ "tried to match " ++ show (quote e)

vsuc :: Value -> Value
vsuc (VNumber n) = VNumber (VSuc n)

vapp :: Value -> Value -> Value
vapp (VError)     _      = VError
vapp _            VError = VError    
vapp (VAbs f)     v      = f v
vapp (VNeutral n) v      = VNeutral (NApp n v)

vfree :: Name -> Value
vfree n = VNeutral (NFree n)

quote :: Value -> ExpL
quote = quote' 0

quote' :: Int -> Value -> ExpL
quote' i (VAbs f)           = AbsL (quote' (i + 1) (f (vfree (Quote i))))
quote' i (VNumber VZero)    = ZeroL
quote' i (VNumber (VSuc n)) = SucL (quote' i (VNumber n)) 
quote' i (VError)           = Error 
quote' i (VNeutral n)       = neutralQuote i n

neutralQuote :: Int -> Neutral -> ExpL
neutralQuote i (NFree x)  = boundfree i x
neutralQuote i (NApp n v) = AppL (neutralQuote i n) (quote' i v) 

boundfree :: Int -> Name -> ExpL
boundfree i (Quote k) = Bound (i - k - 1)
boundfree i  x        = Free x  

expStoExpL :: ExpS -> Context -> [String] -> ExpL
expStoExpL (VarS v)               ctx s = if elem v s then Error else Bound (find v ctx)
expStoExpL (ZeroS)                ctx s = ZeroL
expStoExpL (SucS e)               ctx s = SucL (expStoExpL e ctx s)
expStoExpL (AbsS v t e)           ctx s = AbsL (expStoExpL e ((v, t):ctx) s)
expStoExpL (AppS e1 e2)           ctx s = AppL (expStoExpL e1 ctx s) (expStoExpL e2 ctx s)
expStoExpL (MatchS e1 e2 (v, e3)) ctx s = MatchL (expStoExpL e1 ctx s) 
                                            (expStoExpL e2 ctx s) (expStoExpL e3 ((v, NatS):ctx) s)

find :: String -> Context -> Int
find s ((s', t):xs)
  | s == s'   = 0
  | otherwise = 1 + find s xs

progStoExpL' :: ProgS -> Context -> ExpL
progStoExpL' (MainS t e)            ctx = expStoExpL e ctx []
progStoExpL' (DeclS (FunS s t e) p) ctx = AppL (AbsL (progStoExpL' p ((s, t):ctx))) (expStoExpL e (ctx) [s])

progStoExpL :: ProgS -> ExpL
progStoExpL p = progStoExpL' p []

prettyPrint :: ExpL -> String
prettyPrint (Error)  = "out of fuel"
prettyPrint (ZeroL)  = "0"
prettyPrint (SucL e) = show (1 + calc e)
  where 
    calc (ZeroL)  = 0
    calc (SucL e) = 1 + calc e
prettyPrint e     = show e