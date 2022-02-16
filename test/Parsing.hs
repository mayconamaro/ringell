module Parsing where

import Parser
import Lexer
import Semantic
import Test.HUnit

powerTXT :: String
powerTXT = "sum : nat -> nat -> nat \
  \ \nsum = \\x : nat \\y : nat match x zero -> y suc w -> sum . w . suc y \
  \ \n \
  \ \nmult : nat -> nat -> nat \
  \ \nmult = \\x : nat \\y : nat match x zero -> 0 suc w -> sum . y . (mult . w . y) \
  \ \n \
  \ \npower : nat -> nat -> nat \
  \ \npower = \\x : nat \\y : nat match y zero -> 1 suc w -> mult . x . (power . x . w) \
  \ \n \
  \ \nmain : nat \
  \ \nmain = power . 2 . 3"

powerAST :: Prog
powerAST = Decl (Fun "sum" (ArrowT NatT (ArrowT NatT NatT)) "sum" (Abs "x" NatT (Abs "y" NatT (MatchE (Var "x") 
  (Var "y") ("w",App (App (Var "sum") (Var "w")) (SucE (Var "y"))))))) (Decl (Fun "mult" (ArrowT NatT (ArrowT NatT 
  NatT)) "mult" (Abs "x" NatT (Abs "y" NatT (MatchE (Var "x") (NumE 0) ("w",App (App (Var "sum") (Var "y")) 
  (BracketE (App (App (Var "mult") (Var "w")) (Var "y")))))))) (Decl (Fun "power" (ArrowT NatT (ArrowT NatT NatT)) 
  "power" (Abs "x" NatT (Abs "y" NatT (MatchE (Var "y") (NumE 1) ("w",App (App (Var "mult") (Var "x")) (BracketE 
  (App (App (Var "power") (Var "x")) (Var "w")))))))) (MainF NatT (App (App (Var "power") (NumE 2)) (NumE 3)))))

powerProgS :: ProgS
powerProgS = DeclS (FunS "sum" (ArrowS NatS (ArrowS NatS NatS)) (AbsS "x" NatS (AbsS "y" NatS (MatchS (VarS "x") 
  (VarS "y") ("w",AppS (AppS (VarS "sum") (VarS "w")) (SucS (VarS "y"))))))) (DeclS (FunS "mult" (ArrowS NatS 
  (ArrowS NatS NatS)) (AbsS "x" NatS (AbsS "y" NatS (MatchS (VarS "x") ZeroS ("w",AppS (AppS (VarS "sum") 
    (VarS "y")) 
  (AppS (AppS (VarS "mult") (VarS "w")) (VarS "y"))))))) (DeclS (FunS "power" (ArrowS NatS (ArrowS NatS NatS)) 
  (AbsS "x" NatS (AbsS "y" NatS (MatchS (VarS "y") (SucS ZeroS) ("w",AppS (AppS (VarS "mult") (VarS "x")) (AppS 
  (AppS (VarS "power") (VarS "x")) (VarS "w"))))))) (MainS NatS (AppS (AppS (VarS "power") (SucS (SucS ZeroS))) 
  (SucS (SucS (SucS ZeroS)))))))

testProgram :: Test
testProgram = TestCase (assertEqual "power.rl parsing" (parse $ alexScanTokens powerTXT) powerAST)

testProgS :: Test
testProgS = TestCase (assertEqual "power.rl progS" (progToProgS [] powerAST) powerProgS)

allUnitTests :: Test
allUnitTests = TestList [testProgram, testProgS]