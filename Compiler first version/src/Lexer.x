{
module Lexer where
}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

  $white+                 ;
  \.				              { \s -> Dot }
  match					          { \s -> Match }
  zero					          { \s -> Zero }
  suc                     { \s -> Suc }
  \\                      { \s -> Backslash }
  $digit+				          { \s -> Num (read s) }
  \(                      { \s -> LParen }
  \)                      { \s -> RParen } 
  \=                      { \s -> Assign }
  \:                      { \s -> Colon }
  "->"                    { \s -> Arrow }
  nat                     { \s -> Nat }
  main                    { \s -> Main }
  $alpha [$alpha $digit]* { \s -> ID s }

{
-- Each action has type :: String -> Token

-- The token type:
data Token = ID String 
           | Colon
           | Arrow
           | Nat
           | Num Int
           | Match
           | Zero
           | Suc
           | Backslash
           | Assign 
           | Dot
           | LParen 
           | RParen
           | Main
          deriving (Eq,Show)
}