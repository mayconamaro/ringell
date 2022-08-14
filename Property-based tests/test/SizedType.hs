module SizedType where

import ExpR

data Nat  = TZero 
          | TSuc Nat
          deriving (Eq, Ord)

convert :: Nat -> Int
convert TZero = 0
convert (TSuc n) = 1 + convert n

instance Show Nat where
  show = show . convert

data Type = TNat Nat 
          | Type :-> Type
          deriving (Eq, Show)

-- Subtyping
instance Ord Type where
  (TNat x)    <= (TNat y)    = x <= y
  (t1 :-> t2) <= (t3 :-> t4) = t3 <= t1 && t2 <= t4
  _ <= _ = False