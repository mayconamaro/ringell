module EvalL where

import Control.Monad
import Semantic

data ExpL = ZeroL
          | ErrorL
          | SucL ExpL
          | Bound Int
          | AbsL ExpL
          | AppL ExpL ExpL
          | MatchL ExpL ExpL ExpL
          deriving (Eq, Show)

-- Variable Shifting and Substitution
--

shiftTerm :: Int -> ExpL -> ExpL
shiftTerm d = walk 0
  where walk c (Bound x)
          | x >= c               = Bound (x+d)
          | otherwise            = Bound x
        walk c (AbsL t1)         = AbsL (walk (c+1) t1)
        walk c (AppL t1 t2)      = AppL (walk c t1) (walk c t2) 
        walk c (ZeroL)           = ZeroL
        walk c (SucL t)          = SucL (walk c t)
        walk c (MatchL t1 t2 t3) = MatchL (walk c t1) (walk c t2) (walk (c+1) t3)
        walk c (ErrorL)          = ErrorL

substTerm :: Int -> ExpL -> ExpL -> ExpL
substTerm j s = walk 0
  where walk c (Bound x)
          | x == j+c             = s
          | otherwise            = Bound x
        walk c (AbsL t1)         = AbsL (walk (c+1) t1)
        walk c (AppL t1 t2)      = AppL (walk c t1) (walk c t2)
        walk c (ZeroL)           = ZeroL
        walk c (SucL t)          = SucL (walk c t)
        walk c (MatchL t1 t2 t3) = MatchL (walk c t1) (walk c t2) (walk (c+1) t3)
        walk c (ErrorL)          = ErrorL

substTopTerm :: ExpL -> ExpL -> ExpL
substTopTerm s t = shiftTerm (-1) (substTerm 0 (shiftTerm 1 s) t)

-- Evaluation

isValue :: ExpL -> Bool
isValue (AbsL _) = True
isValue (ZeroL)  = True
isValue (SucL t) = isValue t
isValue _        = False

isError :: ExpL -> Bool
isError ErrorL = True
isError _      = False

eval1 :: ExpL -> Maybe ExpL
eval1 (AppL (AbsL t12) v2)
  | isValue v2 = return $ substTopTerm v2 t12
  | isError v2 = return ErrorL
eval1 (AppL t1 t2)
  | isValue t1 = liftM2 AppL (return t1) (eval1 t2)
  | isError t1 = return ErrorL
  | otherwise  = liftM2 AppL (eval1  t1) (return t2)
eval1 (SucL t) 
  | isValue t = Nothing
  | otherwise = liftM SucL (eval1 t)
eval1 (MatchL t1 t2 t3)
  | isValue t1 = case t1 of
      ZeroL  -> return t2
      SucL t -> return $ substTopTerm t t3
  | isError t1 = return ErrorL
  | otherwise  = liftM3 MatchL (eval1 t1) (return t2) (return t3)
eval1 _ = Nothing

eval :: ExpL -> ExpL
eval t =
  case eval1 t of
    Just t' -> eval t'
    Nothing -> t

-- Translating to System L

expStoExpL :: ExpS -> Context -> [String] -> ExpL
expStoExpL (VarS v)               ctx s = if elem v s then ErrorL else Bound (find v ctx)
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
prettyPrint (ErrorL)  = "out of fuel"
prettyPrint (ZeroL)  = "0"
prettyPrint (SucL e) = show (1 + calc e)
  where 
    calc (ZeroL)  = 0
    calc (SucL e) = 1 + calc e
prettyPrint e     = show e