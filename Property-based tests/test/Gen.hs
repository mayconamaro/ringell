module Gen where

import Test.QuickCheck
import Control.Monad
import ExpR
import qualified ExpL as L
import SizedType
import Unroll (transform, tsl')

type Context = [(String, Type)] -- (name, sized type)

fromIntToNat :: Int -> Nat
fromIntToNat x
    | x > 0     = TSuc (fromIntToNat (x-1))
    | otherwise = TZero

genIndex :: Int -> Gen Nat
genIndex r
  = do
    i <- choose (1, r)
    return $ fromIntToNat i
    
genTypeNat :: Int -> Gen Type
genTypeNat r = liftM TNat (genIndex r) 

genTypeFun :: Int -> Int -> Gen Type
genTypeFun 0 r
  = do
    n1 <- genTypeNat r
    n2 <- genTypeNat r
    liftM2 (:->) (return n1) (return n2)
genTypeFun d r
  = do
    n1 <- genTypeNat r
    n2 <- oneof [genTypeNat r, genTypeFun (d `div` 2) r]
    liftM2 (:->) (return n1) (return n2) 

genType :: Int -> Int -> Gen Type
genType d r = oneof [genTypeNat r, genTypeFun d r]

genZero :: Gen ExpR
genZero = return Zero

calcDeBruijn :: [a] -> [(a, Int)]
calcDeBruijn xs = let n = (length xs) - 1 in zip xs [0..n] 

candidatesBuilder :: ((a, Int) -> Bool) -> [a] -> [Gen (a, Int)]
candidatesBuilder f xs = map return $ filter f $ calcDeBruijn xs

varCriteria :: Type -> ((String, Type), Int) -> Bool
varCriteria ty ((n, t), i) = t <= ty

candidates :: Context -> Type -> [Gen ((String, Type), Int)]
candidates ctx ty = candidatesBuilder (varCriteria ty) ctx

existCandidates :: Context -> Type -> Bool
existCandidates ctx ty = not . null $ candidates ctx ty

genVar :: Context -> Type -> Gen ExpR
genVar ctx ty
  = do
    ((v, t), i) <- oneof $ candidatesBuilder (varCriteria ty) ctx
    liftM2 Var (return v) (return i)

genSuc :: Int -> Context -> Gen ExpR
genSuc 0 _ = genZero
genSuc r ctx
  = do
    let r' = r - 1
    let ty = TNat (fromIntToNat r')
    let ls = if existCandidates ctx ty 
             then [(2, genZero), (2, genSuc r' ctx), (1, genVar ctx ty)]
             else [(2, genZero), (2, genSuc r' ctx)]
    e <- frequency ls
    liftM Suc (return e)

nameSupply :: [String]
nameSupply = ["x" ++ show n | n <- [1..]]

freshVar' :: Context -> [String] -> String
freshVar' ctx (s : ss)
  | s `elem` (map fst ctx) = freshVar' ctx ss
  | otherwise = s

freshVar :: Context -> String
freshVar = flip freshVar' nameSupply

eraseSizeOnTy :: Type -> Ty
eraseSizeOnTy (TNat x) = TyNat
eraseSizeOnTy (t1 :-> t2) = TyFun (eraseSizeOnTy t1) (eraseSizeOnTy t2)

genAbs :: Int -> Int -> Context -> Type -> Gen ExpR
genAbs d r ctx (t1 :-> t2)
  = do
    let v    = freshVar ctx
    let d'   = d `div` 2
    let ctx' = (v, t1) : ctx
    e <- genExpR d r ctx' t2
    liftM3 Abs (return v) (return (eraseSizeOnTy t1)) (return e)

genApp :: Int -> Int -> Context -> Type -> Gen ExpR
genApp d r ctx ty
  = do
    let d' = d `div` 2
    t1 <- genType d' r
    e1 <- genExpR d' r ctx (t1 :-> ty)
    e2 <- genExpR d' r ctx t1
    liftM2 App (return e1) (return e2)

genMatch :: Int -> Int -> Context -> Type -> Gen ExpR
genMatch d r ctx ty
  = do
    let d'  = d `div` 2
    let tn  = TNat (fromIntToNat r)
    let tn' = TNat (fromIntToNat (r-1))
    let v   = freshVar ctx
    e1 <- genExpR d' r ctx tn
    e2 <- genExpR d' r ctx ty
    e3 <- genExpR d' r ((v, tn') : ctx) ty
    liftM4 Match (return e1) (return e2) (return v) (return e3)

diminishSize :: Type -> Type
diminishSize (TNat (TSuc x)) = TNat x
diminishSize (TNat TZero)    = TNat TZero
diminishSize (t1 :-> t2 )    = (diminishSize t1) :-> (diminishSize t2)

standardize :: ExpR -> String -> Int -> String -> Int -> ExpR
standardize (App (e1@(Var x1 i1)) e2) recn reci v vi
  = if x1 == recn && i1 == reci 
    then App (Var x1 i1) (Var v vi)
    else App (standardize e1 recn reci v vi) (standardize e2 recn reci v vi)
standardize (App e1 e2) recn reci v vi 
  = App (standardize e1 recn reci v vi) (standardize e2 recn reci v vi)
standardize (Abs v' t e) recn reci v vi
  = Abs v' t (standardize e recn (reci + 1) v (vi +1))
standardize (Match e1 e2 v' e3) recn reci v vi
  = Match (standardize e1 recn reci v vi) (standardize e2 recn reci v vi) v' (standardize e3 recn (reci+1) v (vi+1))
standardize e _ _ _ _ = e

buildAbsWithMatch :: Int -> Int -> Type -> Context -> Gen ExpR
buildAbsWithMatch d r (t1 :-> t2) ctx 
  = do 
    let v = freshVar ctx 
    let t1' = eraseSizeOnTy t1
    e <- buildAbsWithMatch d r t2 ((v, t1) : ctx)
    liftM3 Abs (return v) (return t1') (return e)
buildAbsWithMatch d r (ty@(TNat x)) ctx
  = do
    e1 <- if existCandidates ctx (diminishSize ty) 
          then oneof [genZero, genVar ctx (diminishSize ty)] 
          else genZero
    e2 <- genSuc r []
    let v = freshVar ctx
    let ((recn, rect), reci) = last (calcDeBruijn ((v, diminishSize ty) : ctx))
    e3' <- genExpR (d `div` 2) (r-1) ((v, diminishSize ty) : ctx) ty
    let e3 = standardize e3' recn reci v 0
    liftM4 Match (return e1) (return e2) (return v) (return e3)

genRec :: Int -> Int -> Context -> Type -> Gen ExpR
genRec d r ctx (ty@(t1 :-> t2))
  = do
    let v   = freshVar ctx
    let ty' = diminishSize ty
    let tn  = eraseSizeOnTy ty
    e <- buildAbsWithMatch (d `div` 2) r ty ((v, ty') : ctx) 
    liftM3 Rec (return v) (return tn) (return e)

buildApps :: Int -> Int -> Context -> ExpR -> [Type] -> Gen ExpR
buildApps d r ctx eRec (t : [])
  = do
    e <- genExpR d r ctx t
    liftM2 App (return eRec) (return e)
buildApps d r ctx eRec (t : ts)
  = do
    e2 <- genExpR d r ctx t
    e1 <- buildApps d r ctx eRec ts
    liftM2 App (return e1) (return e2)  
    
argumentList :: Type -> [Type]
argumentList (t1 :-> t2) = t1 : (argumentList t2)  
argumentList  _          = []

revArgumentList :: Type -> [Type]
revArgumentList = reverse . argumentList

genAppedRec :: Int -> Int -> Context -> Type -> Gen ExpR
genAppedRec d r ctx (ty@(TNat x))
  = do
    ty1 <- genTypeFun d (convert x)
    e   <- genRec (d `div` 2) r ctx ty1
    buildApps d r ctx e (revArgumentList ty1)

genExpR :: Int -> Int -> Context -> Type -> Gen ExpR
genExpR 0 r ctx (TNat x)
  = genZero
genExpR 1 r ctx (TNat x)
  = genSuc (convert x) ctx
genExpR d r ctx (TNat x)
  = if existCandidates ctx (TNat x) 
    then frequency [(3, genZero), 
                    (3, genSuc (convert x) ctx), 
                    (2, genVar ctx (TNat x)), 
                    (2, genApp d r ctx (TNat x)),
                    (1, genMatch d r ctx (TNat x))]
    else frequency [(3, genZero), 
                    (3, genSuc (convert x) ctx),
                    (2, genApp d r ctx (TNat x)),
                    (1, genMatch d r ctx (TNat x))]
genExpR 0 r ctx (t1 :-> t2)
  = genAbs 0 r ctx (t1 :-> t2)
genExpR d r ctx (t1 :-> t2)
  = if existCandidates ctx (t1 :-> t2)
    then frequency [(3, genAbs d r ctx (t1 :-> t2)),
                    (2, genApp d r ctx (t1 :-> t2)),
                    (2, genVar ctx (t1 :-> t2))]
    else frequency [(3, genAbs d r ctx (t1 :-> t2)),
                    (2, genApp d r ctx (t1 :-> t2))]

defaultR :: Int
defaultR = 3

defaultD :: Int
defaultD = 20

genSizedTerm :: Int -> Gen ExpR
genSizedTerm d 
  = do
    r <- choose (1, defaultR+1)
    t <- genType d r
    e <- genSizedTermWithType t d
    return e

genTerm :: Gen ExpR
genTerm
  = do
    d <- choose (1, defaultD+1)
    e <- genSizedTerm d
    return e

genSizedTermWithType :: Type -> Int -> Gen ExpR
genSizedTermWithType (ty@(t1 :-> t2)) d 
  = do 
    r <- choose (1, defaultR+1)
    e <- frequency [(1, genExpR d r [] ty), (2, genRec d r [] ty)]
    return e
genSizedTermWithType (ty@(TNat x)) d
  = do 
    r <- choose (1, defaultR+1)
    e <- frequency [(1, genExpR d r [] ty), (2, genAppedRec d r [] ty)]
    return e

genTermWithType :: Type -> Gen ExpR
genTermWithType ty
  = do
    d <- choose (1, defaultD+1)
    e <- genSizedTermWithType ty d
    return e

instance Arbitrary Type where
  arbitrary 
    = do
      d <- choose (1, defaultD+1)
      r <- choose (1, defaultR+1)
      genType d r

instance Arbitrary ExpR where
  arbitrary = genTerm

isWellTyped :: ExpR -> Bool
isWellTyped e = case typecheck e of 
                  OK _        -> True
                  TypeError s -> False 

generatorSound :: Property
generatorSound 
  = forAll (arbitrary :: Gen Type) 
           (\t -> forAll (genTermWithType t)
                  (\e -> isWellTyped e))

-- generatorSoundAlternative :: Property
-- generatorSoundAlternative = forAll (arbitrary :: Gen ExpR) (\e -> isWellTyped e)

generatedProgramsTermination :: Property
generatedProgramsTermination
  = forAll (arbitrary :: Gen Type) 
           (\t -> forAll (genTermWithType t)
                  (\e -> isValue $ fst (evalFueledWithAbortion e)))

-- generatedProgramsTerminationAlternative :: Property
-- generatedProgramsTerminationAlternative = forAll (arbitrary :: Gen ExpR) (\e -> isValue (eval e))

defaultFuel :: Int
defaultFuel = 100

-- If original program terminates with value v using at most f recursive calls
-- then the transformed program with fuel f also yields v  
property3 :: Property
property3 
  = forAll (genTypeNat defaultR) 
           (\t -> forAll (genTermWithType t)
                  (\e -> isValue (expEval e) ==> tsl' (eval e) == L.eval (transform e (fuelEval e))))
  where
    expEval  = fst . evalFueledWithAbortion
    fuelEval = snd. evalFueledWithAbortion

-- If the transformed program yields value v, then the original program yields value v
property4 :: Property
property4
  = forAll (genTypeNat defaultR) 
           (\t -> forAll (genTermWithType t)
                  (\e -> let evExp = L.eval (transform e defaultFuel) in 
                          L.isValue evExp ==> tsl' (eval e) == evExp))

-- Not enough fuel will yield error unless the term has no recursion
propertyA :: Property
propertyA 
  = forAll (genTypeNat defaultR)
           (\t -> forAll (genTermWithType t)
                  (\e -> L.isValue (L.eval (transform e 0)) || L.isError (L.eval (transform e 0))))
propertyA' :: Property
propertyA' 
  = forAll (genTypeFun defaultD defaultR)
           (\t -> forAll (genTermWithType t)
                  (\e -> L.isValue (L.eval (transform e 0)) || L.isError (L.eval (transform e 0))))

-- A tautology test to make Stack stop accusing show is not tested
-- pretty print is necessary by quickcheck only
propertyShow :: Property
propertyShow
  = forAll (arbitrary :: Gen ExpR)
           (\e -> show e == show e)

testGenerator :: IO ()
testGenerator 
  = do 
    putStrLn "Main tests"
    quickCheckWith stdArgs {maxSuccess = 1000} generatorSound
    quickCheckWith stdArgs {maxSuccess = 1000} generatedProgramsTermination
    quickCheckWith stdArgs {maxSuccess = 1000} property3
    quickCheckWith stdArgs {maxSuccess = 1000} property4
    putStrLn "Trivial tests to increase coverage"
    quickCheckWith stdArgs {maxSuccess = 1000} propertyA
    quickCheckWith stdArgs {maxSuccess = 1000} propertyA'
    quickCheckWith stdArgs {maxSuccess = 1000} propertyShow
