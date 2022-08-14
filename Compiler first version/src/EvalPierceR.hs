module EvalPierceR where

import Control.Monad
import Semantic

data ExpR = ZeroR
          | SucR ExpR
          | Bound Int
          | AbsR ExpR
          | AppR ExpR ExpR
          | MatchR ExpR ExpR ExpR
          | RecR ExpR
          deriving (Eq, Show)

-- Variable Shifting and Substitution

shiftTerm :: Int -> ExpR -> ExpR
shiftTerm d = walk 0
  where walk c (Bound x)
          | x >= c               = Bound (x+d)
          | otherwise            = Bound x
        walk c (AbsR t1)         = AbsR (walk (c+1) t1)
        walk c (AppR t1 t2)      = AppR (walk c t1) (walk c t2) 
        walk c (ZeroR)           = ZeroR
        walk c (SucR t)          = SucR (walk c t)
        walk c (MatchR t1 t2 t3) = MatchR (walk c t1) (walk c t2) (walk (c+1) t3)
        walk c (RecR t)          = RecR (walk (c+1) t)

substTerm :: Int -> ExpR -> ExpR -> ExpR
substTerm j s = walk 0
  where walk c (Bound x)
          | x == j+c             = s
          | otherwise            = Bound x
        walk c (AbsR t1)         = AbsR (walk (c+1) t1)
        walk c (AppR t1 t2)      = AppR (walk c t1) (walk c t2)
        walk c (ZeroR)           = ZeroR
        walk c (SucR t)          = SucR (walk c t)
        walk c (MatchR t1 t2 t3) = MatchR (walk c t1) (walk c t2) (walk (c+1) t3)
        walk c (RecR t)          = RecR (walk (c+1) t)

substTopTerm :: ExpR -> ExpR -> ExpR
substTopTerm s t = shiftTerm (-1) (substTerm 0 (shiftTerm 1 s) t)

-- Evaluation

isValue :: ExpR -> Bool
isValue (AbsR _) = True
isValue (ZeroR)  = True
isValue (SucR t) = isValue t
isValue _        = False

eval1 :: ExpR -> Maybe ExpR
eval1 (AppR (AbsR t12) v2)
  | isValue v2 = return $ substTopTerm v2 t12
eval1 (AppR t1 t2)
  | isValue t1 = liftM2 AppR (return t1) (eval1 t2)
  | otherwise  = liftM2 AppR (eval1  t1) (return t2)
eval1 (SucR t) 
  | isValue t = Nothing
  | otherwise = liftM SucR (eval1 t)
eval1 (MatchR t1 t2 t3)
  | isValue t1 = case t1 of
      ZeroR  -> return t2
      SucR t -> return $ substTopTerm t t3
  | otherwise  = liftM3 MatchR (eval1 t1) (return t2) (return t3)
eval1 (RecR t) = return $ substTopTerm (RecR t) t
eval1 _ = Nothing

eval :: ExpR -> ExpR
eval t =
  case eval1 t of
    Just t' -> eval t'
    Nothing -> t



-- Translating to extended System R
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

progStoExpR' :: ProgS -> Context -> ExpR
progStoExpR' (MainS t e)            ctx = expStoExpR e ctx
progStoExpR' (DeclS (FunS s t e) p) ctx = AppR (AbsR (progStoExpR' p ((s, t):ctx))) 
                                               (RecR (expStoExpR e ((s, t):ctx)))

progStoExpR :: ProgS -> ExpR
progStoExpR p = progStoExpR' p []

prettyPrintR :: ExpR -> String
prettyPrintR (ZeroR)  = "0"
prettyPrintR (SucR e) = show (1 + calc e)
  where 
    calc (ZeroR)  = 0
    calc (SucR e) = 1 + calc e
prettyPrintR e     = show e