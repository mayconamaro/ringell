module Generator where

import Semantic
import Test.QuickCheck
import Control.Monad 
import EvalL
import EvalR
import Unroll

genTypeS :: Gen TypeS
genTypeS = sized genTypeS'

genTypeS' :: Int -> Gen TypeS
genTypeS' 0 = return NatS
genTypeS' n | n>0 = 
    oneof [return NatS, liftM2 ArrowS subtype subtype]
  where
    subtype = genTypeS' (n `div` 2)

instance Arbitrary TypeS where
  arbitrary = genTypeS

newName' :: [String] -> Context -> String
newName' (x:xs) ctx = if x `elem` (map fst ctx) then newName' xs ctx else x

newName :: Context -> String
newName = newName' [ 'x' : show n | n <- [0..] ]

genExpS' :: Int -> Context -> TypeS -> Maybe String -> Gen ExpS
genExpS' 0 ctx NatS Nothing 
  | ns == []  = oneof [return ZeroS, return (SucS ZeroS)]
  | otherwise = oneof $ [return ZeroS, return (SucS ZeroS)] ++ x
  where
    ns = filter (((==) NatS) . snd) ctx
    x  = map (return . VarS) (map fst ns)
genExpS' r ctx (ArrowS t1 t2) Nothing
  | ns == []  = liftM (AbsS s t1) subterm
  | otherwise = oneof $ (liftM (AbsS s t1) subterm) : ns'
  where
    comp (v, NatS)           = False
    comp (v, ArrowS t1' t2') = t1' == t1 && t2' == t2
    embed (v, t) = return (VarS v)
    ns  = filter comp ctx 
    ns' = map embed ns
    s   = newName ctx
    subterm = genExpS' r ((s, t1):ctx) t2 Nothing
genExpS' r ctx NatS Nothing
  | ns1==[] && ns2==[] = oneof [return ZeroS, return (SucS ZeroS)]
  | ns1/=[] && ns2==[] = oneof $ [return ZeroS, return (SucS ZeroS)] ++ x ++ y
  | ns1==[] && ns2/=[] = oneof $ [return ZeroS, return (SucS ZeroS)] ++ z
  | otherwise          = oneof $ [return ZeroS, return (SucS ZeroS)] ++ x ++ y ++ z
  where
    ns1 = filter (((==) NatS) . snd) ctx
    ns2 = filter (\(w, ArrowS t1 t2) -> t2 == NatS) (filter (((/=) NatS) . snd) ctx)
    x = map (return . VarS) (map fst ns1)
    s = newName ctx
    h = \w arg1 arg2 -> MatchS (VarS w) arg1 (s, arg2)
    y = map (\w -> liftM2 (h w) subterm1 subterm2) (map fst ns1)
    subterm1 = genExpS' (r `div` 2) ctx NatS Nothing
    subterm2 = genExpS' (r `div` 2) ((s, NatS):ctx) NatS Nothing
    z = map (\(w, ArrowS t1 NatS) -> liftM (AppS (VarS w)) (genExpS' (r-1) ctx t1 Nothing)) ns2
genExpS' 0 ctx NatS (Just recf) 
  | ns == []  = oneof [return ZeroS, return (SucS ZeroS)]
  | otherwise = oneof $ [return ZeroS, return (SucS ZeroS)] ++ x
  where
    ns = filter (((==) NatS) . snd) ctx
    x  = map (return . VarS) (filter (/= recf) (map fst ns))
genExpS' r ctx (ArrowS t1 t2) (Just recf)
  | ns == []  = liftM (AbsS s t1) subterm
  | otherwise = oneof $ (liftM (AbsS s t1) subterm) : ns'
  where
    comp (v, NatS)           = False
    comp (v, ArrowS t1' t2') = (v /= recf) && t1' == t1 && t2' == t2
    embed (v, t) = return (VarS v)
    ns  = filter comp ctx 
    ns' = map embed ns
    s   = newName ctx
    subterm = genExpS' r ((s, t1):ctx) t2 (Just recf)
genExpS' r ctx NatS (Just recf)
  | ns1==[] && ns2==[] = oneof [return ZeroS, return (SucS ZeroS)]
  | ns1/=[] && ns2==[] = oneof $ [return ZeroS, return (SucS ZeroS)] ++ x ++ y
  | ns1==[] && ns2/=[] = oneof $ [return ZeroS, return (SucS ZeroS)] ++ z
  | otherwise          = oneof $ [return ZeroS, return (SucS ZeroS)] ++ x ++ y ++ z
  where
    ns1 = filter (((==) NatS) . snd) ctx
    ns2 = filter (\(w, ArrowS t1 t2) -> w /= recf && t2 == NatS) (filter (((/=) NatS) . snd) ctx)
    x = map (return . VarS) (map fst ns1)
    s = newName ctx
    h = \w arg1 arg2 -> MatchS (VarS w) arg1 (s, arg2)
    y = map (\w -> liftM2 (h w) subterm1 subterm2) (filter (/= recf) (map fst ns1))
    subterm1 = genExpS' (r `div` 2) ctx NatS (Just recf)
    subterm2 = if lookupV ctx recf == ArrowS NatS NatS 
               then return $ AppS (VarS recf) (VarS s) 
               else if lookupV ctx recf == ArrowS NatS (ArrowS NatS NatS) 
                    then liftM (AppS (AppS (VarS recf) (VarS s))) 
                               (genExpS' (r-1) ((s, NatS):ctx) NatS (Just recf))  
                    else genExpS' (r `div` 2) ((s, NatS):ctx) NatS (Just recf)
    z = map (\(w, ArrowS t1 NatS) -> liftM (AppS (VarS w)) (genExpS' (r-1) ctx t1 (Just recf))) ns2

genProgS :: Gen ProgS
genProgS = do 
  r <- choose (0, 10)
  sized (genProgS' [] r)

genProgS' :: Context -> Int -> Int -> Gen ProgS
genProgS' ctx s 0 = liftM (MainS NatS) (genExpS' s ctx NatS Nothing)
genProgS' ctx s r = 
  do 
    let v = newName ctx
    t <- genTypeS
    let h = \arg1 arg2 -> DeclS (FunS v t arg1) arg2 
    liftM2 h (genExpS' s ((v, t):ctx) t (Just v)) (genProgS' ((v, t):ctx) s (r-1))

instance Arbitrary ProgS where
  arbitrary = genProgS

propWellTyped :: ProgS -> Bool
propWellTyped p = (typecheck p []) == Ok 

propOutOfFuel :: ProgS -> Bool
propOutOfFuel p = el /= "out of fuel"
  where
    fexpL = progStoExpL (inlineF [] p 50)
    el    = prettyPrint . quote $ eval fexpL []

propEval :: ProgS -> Bool
propEval p = el == er 
  where
    er    = prettyPrintR . quoteR $ evalR (progStoExpR p) []
    fexpL = progStoExpL (inlineF [] p 50)
    el    = prettyPrint . quote $ eval fexpL []

propSemPreservation :: ProgS -> Property
propSemPreservation p = (propOutOfFuel p) ==> (propEval p)