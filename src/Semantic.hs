module Semantic where

import Parser


-- Translating the AST into 1st Intermediate Representation 
-- No invalid names for function decl+def, peano notation
-- brackets info removed, no duplicate names
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

type Scope = [String]

expToExps :: Exp -> Scope -> ExpS 
expToExps (BracketE e) sc = expToExps e sc 
expToExps (Var v)      sc = VarS v      
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

lookupV :: Context -> String -> TypeS
lookupV c s = lookupV' c s

lookupV' :: Context -> String -> TypeS
lookupV' []             s = error $ "Variable " ++ s ++ " not in scope"
lookupV' ((v, t) : env) s
  | s == v    = t
  | otherwise = lookupV' env s 

-- -- Typecheck
infertypeE :: ExpS -> Context -> TypeS
infertypeE (VarS v)              env = lookupV env v            
infertypeE (ZeroS)                 env = NatS
infertypeE (SucS e)                env 
 | infertypeE e env == NatS = NatS
 | otherwise = error "type error: numbers must be nat" 
infertypeE (AbsS v t e)            env = ArrowS t (infertypeE e ((v, t) : env)) 
infertypeE (AppS e1 e2)            env =
    case infertypeE e1 env of
        ArrowS t1 t2 -> if infertypeE e2 env == t1 
                          then t2
                          else error "type error: function argument does not match the given value"
        _            -> error "type error: numbers cannot be functions"
infertypeE (MatchS e1 e2 (v, e3))  env =
    case infertypeE e1 env of
        NatS -> case infertypeE e2 env of
                  t1 -> if infertypeE e3 ((v , NatS) : env) == t1 
                          then t1
                          else error "type error: pattern match cases must have the same type"
        _    -> error "type error: matching can only occurs with numbers"  

data Status = Ok | Fail String
   deriving (Eq, Show)

typecheck :: ProgS -> Context -> Status
typecheck (MainS t e) env
  | t' == t        = Ok
  | otherwise      = Fail "main type does not match"
    where t' = infertypeE e env
typecheck (DeclS (FunS s t e) p) env
  | t' == t        = typecheck p ((s, t) : env)
  | otherwise      = Fail $ "function type for " ++ s ++ " does not match" 
    where t' = infertypeE e ((s, t) : env)

{- Alternate Pretty Printing for ExpS 
showExpS :: ExpS -> Int -> String
showExpS (VarS s)               n = "Var " ++ s ++ "\n"
showExpS (ZeroS)                n = "Zero\n"
showExpS (SucS e)               n = "Suc\n" ++ replicate (2*n + 2) ' ' ++ showExpS e (n+1)
showExpS (AbsS v t e)           n = "Abs " ++ v ++ " " ++ show t ++ "\n" 
                                      ++ replicate (2*n + 2) ' ' ++ showExpS e (n+1)
showExpS (AppS e1 e2)           n = "App\n" 
                                      ++ replicate (2*n + 2) ' ' ++ showExpS e1 (n+1)
                                      ++ replicate (2*n + 2) ' ' ++ showExpS e2 (n+1)
showExpS (MatchS e1 e2 (v, e3)) n = "Match\n" 
                                      ++ replicate (2*n + 2) ' ' ++ showExpS e1 (n+1) 
                                      ++ replicate (2*n + 2) ' ' ++ showExpS e2 (n+1)
                                      ++ replicate (2*n + 2) ' ' ++ "(" ++ v ++ ", " ++ showExpS e3 (n+1) 
                                      ++ replicate (2*n + 2) ' ' ++ ")\n"

instance Show ExpS where
  show e = showExpS e 0
  -}