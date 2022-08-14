module Generation where

import System.Random
import Semantic 

randSeed :: IO Int
randSeed = randomIO

listOfRands :: Int -> [Double]
listOfRands seed = randoms (mkStdGen seed) :: [Double] 

genType' :: Double -> [Double] -> (TypeS, [Double])
genType' p xs 
 | head xs <= p = (NatS, tail xs)
 | otherwise    = (ArrowS t1 t2, tail xs2)
  where
    (t1, xs1) = genType' (p+0.15) (tail xs)
    (t2, xs2) = genType' (p+0.15) xs1
         
prettyPrintType :: TypeS -> String
prettyPrintType NatS             = "nat"
prettyPrintType (ArrowS NatS t2) = prettyPrintType NatS ++ " -> " ++ prettyPrintType t2
prettyPrintType (ArrowS t1 t2)   = "(" ++ prettyPrintType t1 ++ ") -> " ++ prettyPrintType t2

genType :: [Double] -> TypeS
genType d = fst $ genType' 0.15 d

data Instance = Indirect 
              | Appl 
              | Lam
              | Const
              | PMatch
              deriving (Eq, Show)

pick :: Double -> [a] -> a
pick d xs = pick' 1 d xs 
  where
    pick' i d xs = if d <= ((fromIntegral i) / l) then xs !! (i-1) else pick' (i+1) d xs   
    l = fromIntegral (length xs)

listOfNames :: [String]
listOfNames = [ 'x' : show n | n <- [0..] ]

newName' :: [String] -> Context -> String
newName' (x:xs) ctx = if x `elem` (map fst ctx) then newName' xs ctx else x

newName :: Context -> String
newName = newName' listOfNames

genTerm' :: [Double] -> Context -> TypeS -> (ExpS, [Double])
genTerm' d []   NatS          = if head d < 0.5 then (ZeroS, tail d) else (SucS ZeroS, tail d) 
genTerm' d []  (ArrowS t1 t2) = (AbsS s (ArrowS t1 t2) e, tail ds)
  where
    s       = newName []
    (e, ds) = genTerm' d [(s, t1)] t2
genTerm' d ctx  NatS          = 
  case pick (head d) [Const, Indirect, PMatch] of
    Const    -> if d !! 1 < 0.5 then (ZeroS, drop 2 d) else (SucS ZeroS, drop 2 d) 
    Indirect -> if ns == [] 
                then if ns2 == [] 
                     then if d !! 1 < 0.5 then (ZeroS, drop 2 d) else (SucS ZeroS, drop 2 d)
                     else let (v, ArrowS t1 t2) = pick (d !! 2) ns2 in
                            let (e', ds') = genTerm' (drop 3 d) ctx t1 in
                              (AppS (VarS v) e', tail ds')
                else (VarS (pick (d !! 1) (map fst ns)), drop 2 d)
    PMatch   -> if ns == []
                then if d !! 1 < 0.5 then (ZeroS, drop 2 d) else (SucS ZeroS, drop 2 d)
                else (MatchS (VarS (pick (d !! 1) (map fst ns))) e1 (s, e2), tail ds2)
  where 
    ns  = filter (((==) NatS) . snd) ctx
    ns1 = filter (((/=) NatS) . snd) ctx
    ns2 = filter (\(v, ArrowS t1 t2) -> t2 == NatS) ns1
    s = newName ctx
    (e1, ds1) = genTerm' (drop 2 d) ctx NatS
    (e2, ds2) = genTerm' (tail ds1) ((s, NatS):ctx) NatS
genTerm' d ctx (ArrowS t1 t2) =
  case pick (head d) [Lam, Indirect] of
    Lam -> (AbsS s (ArrowS t1 t2) e , tail ds)
    Indirect -> if ns == []
                then (AbsS s (ArrowS t1 t2) e, tail d)
                else let (v, ArrowS t1' t2') = pick (d !! 1) ns in
                        if t1' == t1 && t2' == t2 
                        then (VarS v, drop 2 d)
                        else (AbsS s (ArrowS t1 t2) e, tail d) 
  where 
    s       = newName ctx
    (e, ds) = genTerm' d ((s, t1):ctx) t2 
    ns      = filter (((/=) NatS) . snd) ctx

genTerm :: [Double] -> Context -> TypeS -> ExpS
genTerm d c t = fst $ genTerm' d c t 

genProg :: [Double] -> Int -> Context -> ProgS
genProg d 0 ctx = MainS NatS (genTerm d ctx NatS)
genProg d r ctx = DeclS (FunS s t e) (genProg ds (r-1) ((s, t):ctx))
  where
    s = newName ctx
    t  = genType d 
    e  = genTerm (drop 10 d) ((s, t):ctx) t
    ds = drop 25 d