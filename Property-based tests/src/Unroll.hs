module Unroll where

import ExpR
import qualified ExpL as L 

inl :: ExpR -> String -> ExpR -> ExpR
inl (Var v' i)          v e = if v == v' then e else (Var v' i)
inl (Zero)              v e = Zero
inl (Suc e')            v e = Suc (inl e' v e)
inl (Abs v' t e')       v e = Abs v' t (inl e' v e)
inl (App e1 e2)         v e = App (inl e1 v e) (inl e2 v e)
inl (Match e1 e2 v' e3) v e = Match (inl e1 v e) (inl e2 v e) v' (inl e3 v e)

exp' :: ExpR -> String -> ExpR -> Int -> ExpR
exp' e' _ _ 0 = e'
exp' e' v e n = inl (exp' e' v e (n-1)) v e

expn :: ExpR -> Int -> ExpR
expn (Rec v t e) n = Rec v t (exp' e v e n)
expn (App e1 e2) n = App (expn e1 n) e2
expn e           _ = e

elim :: ExpR -> String -> L.ExpL
elim (Var v' i)          v = if v == v' then L.Error else L.Var v' i
elim (Zero)              v = L.Zero
elim (Suc e)             v = L.Suc (elim e v)
elim (Abs v' t e)        v = L.Abs v' t (elim e v)
elim (App e1 e2)         v = L.App (elim e1 v) (elim e2 v)
elim (Match e1 e2 v' e3) v = L.Match (elim e1 v) (elim e2 v) v' (elim e3 v)

tsl' :: ExpR -> L.ExpL
tsl' (Var v' i)          = L.Var v' i
tsl' (Zero)              = L.Zero
tsl' (Suc e)             = L.Suc (tsl' e)
tsl' (Abs v' t e)        = L.Abs v' t (tsl' e)
tsl' (App e1 e2)         = L.App (tsl' e1) (tsl' e2)
tsl' (Match e1 e2 v' e3) = L.Match (tsl' e1) (tsl' e2) v' (tsl' e3)

tsl :: ExpR -> L.ExpL
tsl (Rec v t e) = elim e v
tsl (App e1 e2) = L.App (tsl e1) (tsl' e2)
tsl e           = tsl' e

transform :: ExpR -> Int -> L.ExpL
transform e f = tsl (expn e f)