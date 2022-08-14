module ExpR where

import Control.Monad

data Ty = TyNat 
        | TyFun Ty Ty
        deriving (Eq, Show)

data ExpR = Zero
          | Var String Int
          | Suc ExpR
          | Abs String Ty ExpR
          | App ExpR ExpR
          | Match ExpR ExpR String ExpR
          | Rec String Ty ExpR
          deriving (Eq, Show)

-- TypeChecking --
type Env = [Ty]

type ErrorMsg = String
data Status = OK Ty 
            | TypeError ErrorMsg
            deriving (Eq) --, Show

partialIf :: Bool -> a -> a
partialIf True = id

typecheck' :: Env -> ExpR -> Status
typecheck' env  Zero   = OK TyNat
typecheck' env (Suc e) 
    = case typecheck' env e of
        OK TyNat    -> OK TyNat
        -- OK _        -> TypeError "Suc argument is not a nat"
        -- TypeError s -> TypeError s
typecheck' env (Var v i)
    = partialIf (i < length env) (OK (env !! i)) --if i < length env then OK (env !! i) else TypeError "Variable not defined"
typecheck' env (Abs v t e)
    = case typecheck' (t:env) e of
        OK t' -> OK (TyFun t t')
        -- TypeError s -> TypeError s
typecheck' env (App e1 e2) 
    = case (typecheck' env e1, typecheck' env e2) of
        (OK (TyFun t1 t2), OK t3) -> partialIf (t1 == t3) (OK t2)-- if t1 == t3 then OK t2 else TypeError "Function argument does not match"
        -- (OK TyNat, _) -> TypeError "Cannot apply nat values to other terms"
        -- (TypeError s, _) -> TypeError s
        -- (_, TypeError s) -> TypeError s
typecheck' env (Rec v t e)
    = case typecheck' (t:env) e of
        OK t' -> partialIf (t == t') (OK t) -- if t == t' then OK t else TypeError "Recursive definition type mismatch"
        -- TypeError s -> TypeError s
typecheck' env (Match e1 e2 v e3)
    = case typecheck' env e1 of 
        OK TyNat -> case (typecheck' env e2, typecheck' (TyNat:env) e3) of
                    (OK t1, OK t2) -> partialIf (t1 == t2) (OK t1) -- if t1 == t2 then OK t1 else TypeError "Match branches have different types"
                    -- (TypeError s, _) -> TypeError s
                    -- (_, TypeError s) -> TypeError s
        -- OK (TyFun _ _) -> TypeError "Cannot pattern match over a function"
        -- TypeError s    -> TypeError s

typecheck :: ExpR -> Status
typecheck = typecheck' []

-- Substitution  --

shiftTerm :: Int -> ExpR -> ExpR
shiftTerm d = walk 0
  where walk c (Var v x)          = partialIf (x < c) (Var v x)
          -- | x >= c                = Var v (x+d)
          -- | otherwise             = Var v x
        walk c (Abs v ty t1)      = Abs v ty (walk (c+1) t1)
        walk c (App t1 t2)        = App (walk c t1) (walk c t2) 
        walk c (Zero)             = Zero
        walk c (Suc t)            = Suc (walk c t)
        walk c (Match t1 t2 v t3) = Match (walk c t1) (walk c t2) v (walk (c+1) t3)
        walk c (Rec v ty t)       = Rec v ty (walk (c+1) t)

substTerm :: Int -> ExpR -> ExpR -> ExpR
substTerm j s = walk 0
  where walk c (Var v x)
          | x == j+c              = s
          | otherwise             = Var v x
        walk c (Abs v ty t1)      = Abs v ty (walk (c+1) t1)
        walk c (App t1 t2)        = App (walk c t1) (walk c t2)
        walk c (Zero)             = Zero
        walk c (Suc t)            = Suc (walk c t)
        walk c (Match t1 t2 v t3) = Match (walk c t1) (walk c t2) v (walk (c+1) t3)
        walk c (Rec v ty t)       = Rec v ty (walk (c+1) t)

substTopTerm :: ExpR -> ExpR -> ExpR
substTopTerm s t = shiftTerm (-1) (substTerm 0 (shiftTerm 1 s) t)

-- Evaluation --

isValue :: ExpR -> Bool
isValue (Abs _ _ _) = True
isValue (Zero)  = True
isValue (Suc t) = isValue t
isValue _       = False

eval1 :: ExpR -> Maybe ExpR
eval1 (App (Abs s ty t12) v2)
  | isValue v2 = return $ substTopTerm v2 t12
eval1 (App t1 t2)
  | isValue t1 = liftM2 App (return t1) (eval1 t2)
  | otherwise  = liftM2 App (eval1  t1) (return t2)
eval1 (Suc t) 
  | isValue t = Nothing
  | otherwise = liftM Suc (eval1 t)
eval1 (Match t1 t2 s t3)
  | isValue t1 = case t1 of
      Zero  -> return t2
      Suc t -> return $ substTopTerm t t3
  | otherwise   = liftM4 Match (eval1 t1) (return t2) (return s) (return t3)
eval1 (Rec s ty t) = return $ substTopTerm (Rec s ty t) t
eval1 _ = Nothing

eval1F :: ExpR -> (Maybe ExpR, Bool)
eval1F (App (Abs s ty t12) v2)
  | isValue v2 = (return $ substTopTerm v2 t12, False)
eval1F (App t1 t2)
  | isValue t1 = (liftM2 App (return t1) (fst $ eval1F t2), snd $ eval1F t2)
  | otherwise  = (liftM2 App (fst $ eval1F t1) (return t2), snd $ eval1F t1)
eval1F (Suc t) 
  | isValue t = (Nothing, False)
  | otherwise = (liftM Suc (fst $ eval1F t), snd $ eval1F t)
eval1F (Match t1 t2 s t3)
  | isValue t1 = case t1 of
      Zero  -> (return t2, False)
      Suc t -> (return $ substTopTerm t t3, False)
  | otherwise   = (liftM4 Match (fst $ eval1F t1) (return t2) (return s) (return t3), snd $ eval1F t1)
eval1F (Rec s ty t) = (return $ substTopTerm (Rec s ty t) t, True)
eval1F _ = (Nothing, False)

eval :: ExpR -> ExpR
eval t =
  case eval1 t of
    Just t' -> eval t'
    Nothing -> t

-- evalFueled :: ExpR -> (ExpR, Int)
-- evalFueled t =
--   case eval1F t of 
--     (Just t', True)  -> (fst $ evalFueled t', (snd $ evalFueled t') + 1)
--     (Just t', False) -> evalFueled t'
--     (Nothing, _)     -> (t, 0)

defaultLimit :: Int
defaultLimit = 5000

evalFueledWithAbortion' :: ExpR -> Int -> (ExpR, Int)
evalFueledWithAbortion' t f = if f >= defaultLimit then (t, f) 
  else
    case eval1F t of 
      (Just t', True)  -> let x = evalFueledWithAbortion' t' (f+1) in (fst x, (snd x) + 1)
      (Just t', False) -> evalFueledWithAbortion' t' f
      (Nothing, _)     -> (t, 0)

evalFueledWithAbortion :: ExpR -> (ExpR, Int)
evalFueledWithAbortion t = evalFueledWithAbortion' t 0

-- -- Example Expressions --
-- exsum :: ExpR
-- exsum = Rec "sum" (TyFun TyNat (TyFun TyNat TyNat)) (Abs "x" (TyNat) (Abs "y" (TyNat)
--                 (Match 
--                     (Var "x" 1) 
--                     (Var "y" 0) 
--                     "w" (App 
--                             (App 
--                                 (Var "sum" 3) 
--                                 (Var "w" 0)) 
--                             (Suc (Var "y" 1))))))

-- ex3plus4 :: ExpR
-- ex3plus4 
--     = App 
--         (App 
--             (Rec "sum" (TyFun TyNat (TyFun TyNat TyNat)) (Abs "x" (TyNat) (Abs "y" (TyNat)
--                 (Match 
--                     (Var "x" 1) 
--                     (Var "y" 0) 
--                     "w" (App 
--                             (App 
--                                 (Var "sum" 3) 
--                                 (Var "w" 0)) 
--                             (Suc (Var "y" 1))))))) 
--             (Suc (Suc (Suc (Suc Zero))))) 
--         (Suc (Suc (Suc (Suc Zero))))

-- instance Show Ty where
--     show TyNat = "nat"
--     show (TyFun (TyFun t1 t2) t3) = "("++ show (TyFun t1 t2) ++") -> " ++ show t3
--     show (TyFun t1 t2) = show t1 ++ " -> " ++ show t2