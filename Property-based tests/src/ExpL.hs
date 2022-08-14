module ExpL where

import Control.Monad

import ExpR (Ty(TyNat, TyFun), partialIf)

data ExpL = Zero
          | Error
          | Var String Int
          | Suc ExpL
          | Abs String Ty ExpL
          | App ExpL ExpL
          | Match ExpL ExpL String ExpL
          deriving (Eq)--, Show

-- Substitution  --

shiftTerm :: Int -> ExpL -> ExpL
shiftTerm d = walk 0
  where walk c (Var v x)          = partialIf (x < c) (Var v x)
          -- | x >= c                = Var v (x+d)
          -- | otherwise             = Var v x
        walk c (Abs v ty t1)      = Abs v ty (walk (c+1) t1)
        walk c (App t1 t2)        = App (walk c t1) (walk c t2) 
        walk c (Zero)             = Zero
        walk c (Error)            = Error
        walk c (Suc t)            = Suc (walk c t)
        walk c (Match t1 t2 v t3) = Match (walk c t1) (walk c t2) v (walk (c+1) t3)

substTerm :: Int -> ExpL -> ExpL -> ExpL
substTerm j s = walk 0
  where walk c (Var v x)
          | x == j+c              = s
          | otherwise             = Var v x
        walk c (Abs v ty t1)      = Abs v ty (walk (c+1) t1)
        walk c (App t1 t2)        = App (walk c t1) (walk c t2)
        walk c (Zero)             = Zero
        walk c (Suc t)            = Suc (walk c t)
        walk c (Match t1 t2 v t3) = Match (walk c t1) (walk c t2) v (walk (c+1) t3)
        walk c (Error)            = Error

substTopTerm :: ExpL -> ExpL -> ExpL
substTopTerm s t = shiftTerm (-1) (substTerm 0 (shiftTerm 1 s) t)

-- Evaluation --

isValue :: ExpL -> Bool
isValue (Abs _ _ _) = True
isValue (Zero)  = True
isValue (Suc t) = isValue t
isValue _       = False

isError :: ExpL -> Bool
isError = (==) Error

eval1 :: ExpL -> Maybe ExpL
eval1 (App (Abs s ty t12) v2)
  | isValue v2 = return $ substTopTerm v2 t12
  | isError v2 = return Error
eval1 (App t1 t2)
  | isValue t1 = liftM2 App (return t1) (eval1 t2)
  | isError t1 = return Error
  | otherwise  = liftM2 App (eval1  t1) (return t2)
eval1 (Suc t) 
  | isValue t = Nothing
  | isError t = return Error
  | otherwise = liftM Suc (eval1 t)
eval1 (Match t1 t2 s t3)
  | isValue t1 = case t1 of
      Zero  -> return t2
      Suc t -> return $ substTopTerm t t3
  | isError t1 = return Error
  | otherwise   = liftM4 Match (eval1 t1) (return t2) (return s) (return t3)
eval1 _ = Nothing

eval :: ExpL -> ExpL
eval t =
  case eval1 t of
    Just t' -> eval t'
    Nothing -> t