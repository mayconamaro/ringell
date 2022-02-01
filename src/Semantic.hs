module Semantic where

import Parser


-- Translating the AST into 1st Intermediate Representation 
-- No invalid names for function decl+def, peano notation
-- brackets info removed, no duplicate names
data ExpS 
    = VarS String Int
    | ZeroS  
    | SucS ExpS
    | AbsS String TypeS ExpS
    | AppS ExpS ExpS
    | MatchS ExpS ExpS (String, ExpS)
    deriving (Eq, Show)

data TypeS 
    = NatS
    | ArrowS TypeS TypeS
    deriving (Eq, Show)

data FunS 
    = FunS String TypeS ExpS
    deriving (Eq, Show)

data ProgS 
    = DeclS FunS ProgS 
    | MainS TypeS ExpS      
    deriving (Eq, Show)

typeToTypes :: Type -> TypeS
typeToTypes (BracketT t)   = typeToTypes t
typeToTypes  NatT          = NatS
typeToTypes (ArrowT t1 t2) = ArrowS (typeToTypes t1) (typeToTypes t2)

type Scope = [String]

expToExps :: Exp -> Scope -> ExpS 
expToExps (BracketE e) sc = expToExps e sc 
expToExps (Var v)      sc = VarS v (-1)       
expToExps (NumE 0)     sc = ZeroS
expToExps (NumE x)     sc = SucS (expToExps (NumE (x-1)) sc)
expToExps (SucE e)     sc = SucS (expToExps e sc)       
expToExps (Abs v t e)  sc 
  = if elem v sc 
       then error $ "Variable " ++ v ++ " is already used" 
       else AbsS v (typeToTypes t) (expToExps e (v:sc))      
expToExps (App e1 e2)  sc = AppS (expToExps e1 sc) (expToExps e2 sc)        
expToExps (MatchE e1 e2 (v, e3)) sc 
  = if elem v sc 
       then error $ "Variable " ++ v ++ " is already used"
       else MatchS (expToExps e1 sc) (expToExps e2 sc) (v, expToExps e3 (v:sc))

funToFuns :: Fun -> Scope -> FunS 
funToFuns (Fun s1 t s2 e) sc
  | s1 == s2 = if elem s1 sc 
                  then error $ "Function name " ++ s1 ++ " is already used" 
                  else FunS s1 (typeToTypes t) (expToExps e (s1:sc))
  | otherwise = error "A function name in signature does not match its definition"

progToProgS :: Scope -> Prog -> ProgS
progToProgS sc (MainF t e) = MainS (typeToTypes t) (expToExps e sc)
progToProgS sc (Decl f@(Fun s _ _ _) p) = DeclS (funToFuns f sc) (progToProgS (s:sc) p) 

-- Typechecking

type Context = [(String, TypeS)]

lookupV :: Context -> String -> (TypeS, Int)
lookupV c s = lookupV' c s 0

lookupV' :: Context -> String -> Int -> (TypeS, Int)
lookupV' []             s _ = error $ "Variable " ++ s ++ " not in scope"
lookupV' ((v, t) : env) s n
  | s == v    = (t, n)
  | otherwise = lookupV' env s (n+1)

-- -- Typecheck
infertypeE :: ExpS -> Context -> (ExpS, TypeS)
infertypeE (VarS v _)              env = (VarS v i, t)
  where (t, i) = lookupV env v            
infertypeE (ZeroS)                 env = (ZeroS, NatS)
infertypeE (SucS e)                env 
 | t == NatS = (SucS e', NatS) 
 | otherwise = error "type error: numbers must be nat"
   where (e', t) = infertypeE e env
infertypeE (AbsS v t e)            env = (AbsS v t e', ArrowS t t') 
 where (e', t') = infertypeE e ((v, t) : env)
infertypeE (AppS e1 e2)            env =
    case infertypeE e1 env of
        (e, ArrowS t1 t2) -> if snd (infertypeE e2 env) == t1 
                                 then (AppS e (fst (infertypeE e2 env)), t2) 
                                 else error "type error: argument does not match value"
        _                 -> error "type error: numbers cannot be functions"
infertypeE (MatchS e1 e2 (v, e3))  env =
    case infertypeE e1 env of
        (e, NatS) -> case infertypeE e2 env of
                  (e', t1) -> if snd (infertypeE e3 ((v , NatS) : env)) == t1 
                                 then (MatchS e e' (v, (fst (infertypeE e3 ((v , NatS) : env)))) , t1) 
                                 else error "type error: match cases must have the same type"
        _    -> error "type error: matching can only occurs with numbers"  

typecheck :: ProgS -> Context -> ProgS
typecheck (MainS t e) env
  | t' == t        = MainS t e' 
  | otherwise      = error "main type does not match"
    where (e', t') = infertypeE e env
typecheck (DeclS (FunS s t e) p) env
  | t' == t        = DeclS (FunS s t e') (typecheck p ((s, t) : env))
  | otherwise      = error $ "function type for " ++ s ++ " does not match" 
    where (e', t') = infertypeE e ((s, t) : env)