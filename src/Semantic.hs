module Semantic where

import Parser


-- Translating the AST into 1st Intermediate Representation 
-- No invalid names for function decl+def, peano notation
-- brackets info removed
data ExpS 
    = VarS String
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

expToExps :: Exp -> ExpS 
expToExps (BracketE e) = expToExps e
expToExps (Var v)      = VarS v        
expToExps (NumE 0)     = ZeroS
expToExps (NumE x)     = SucS (expToExps (NumE (x-1)))
expToExps (SucE e)     = SucS (expToExps e)       
expToExps (Abs v t e)  = AbsS v (typeToTypes t) (expToExps e)      
expToExps (App e1 e2)  = AppS (expToExps e1) (expToExps e2)        
expToExps (MatchE e1 e2 (v, e3)) = MatchS (expToExps e1) (expToExps e2) (v, expToExps e3)

funToFuns :: Fun -> FunS 
funToFuns (Fun s1 t s2 e)
  | s1 == s2 = FunS s1 (typeToTypes t) (expToExps e)
  | otherwise = error "A function declaration does not match its implementation"

progToProgS :: Prog -> ProgS
progToProgS (MainF t e) = MainS (typeToTypes t) (expToExps e)
progToProgS (Decl f p)  = DeclS (funToFuns f) (progToProgS p)

-- (Var v)              
-- (NumE n)          
-- (SucE e)               
-- (Abs v t e)          
-- (App e1 e2)           
-- (MatchE e1 e2 (v, e3)) 
-- (BracketE e)       

-- Typechecking

type Context = [(String, TypeS)]

lookupV :: Context -> String -> TypeS
lookupV []              s = error $ "Variable " ++ s ++ " not in scope"
lookupV ((v, t) : env) s
  | s == v    = t
  | otherwise = lookupV env s

-- -- Typecheck
infertypeE :: ExpS -> Context -> TypeS
infertypeE (VarS v)                env = lookupV env v            
infertypeE (ZeroS)                 env = NatS
infertypeE (SucS e)                env
 | infertypeE e env == NatS = NatS
 | otherwise = error "type error: numbers must be nat"
infertypeE (AbsS v t e)            env = ArrowS t (infertypeE e ((v, t) : env)) 
infertypeE (AppS e1 e2)            env =
    case infertypeE e1 env of
        ArrowS t1 t2 -> if infertypeE e2 env == t1 then t2 else error "type error: argument does not match value"
        _            -> error "type error: numbers cannot be functions"
infertypeE (MatchS e1 e2 (v, e3))  env =
    case infertypeE e1 env of
        NatS -> case infertypeE e2 env of
                  t1 -> if infertypeE e3 ((v , NatS) : env) == t1 then t1 else error "type error: match cases must have the same type"
        _    -> error "type error: matching can only occurs with numbers"  

typecheck :: ProgS -> Context -> ()
typecheck (MainS t e) env
  | infertypeE e env == t = () 
  | otherwise             = error "main type does not match"
typecheck (DeclS (FunS s t e) p) env
  | infertypeE e ((s, t) : env) == t = typecheck p ((s, t) : env)
  | otherwise                        = error $ "function type for " ++ s ++ " does not match" 


-- data ExpS = ZeroS 
--           | SucS ExpS
--           | VarS String Int  
--           | AbsS ExpS 
--           | AppS ExpS ExpS
--           | MatchS ExpS ExpS ExpS

-- type FunDecl = (String, Type, ExpS)

-- type AST = [FunDecl] 

-- -- substitute variables for numbers
-- type Scope = [String]

-- lookupV :: Scope -> String -> Int
-- lookupV []       s = error $ "Variable " ++ s ++ " not in scope" 
-- lookupV (s : cs) v = if v == s then 0 else 1 + lookupV cs v

-- unnameVarsP :: Prog -> Scope -> Prog
-- unnameVarsP (MainF t e) s =
-- unnameVarsP (Decl (Fun f1 t f2 e) p)  s 
--   = if f1 == f2 then (Decl (Fun "" t "" (unnameVarsE e (s))))

-- unnameVarsE :: Exp -> Scope -> Exp
-- unnameVarsE (Var v)               s  = Var (show (lookupV s v))
-- unnameVarsE (NumE n)               _ = NumE n
-- unnameVarsE (SucE e)               s = SucE (unnameVarsE e s)
-- unnameVarsE (Abs v e)              s = Abs "" (unnameVarsE e (v : s))
-- unnameVarsE (App e1 e2)            s = App (unnameVarsE e1 s) (unnameVarsE e2 s)
-- unnameVarsE (MatchE e1 e2 (v, e3)) s = MatchE (unnameVarsE e1 s) (unnameVarsE e2 s) ("", unnameVarsE e3 (v:s))
-- unnameVarsE (BracketE e)           s = BracketE (unnameVarsE e s)