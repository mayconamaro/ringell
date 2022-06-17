x0 : nat
x0 = x0

x1 : (nat -> nat) -> nat
x1 = \x2 : nat -> nat 0

x2 : nat -> nat -> nat
x2 = \x3 : nat \x4 : nat match x0 zero -> x4 suc x5 -> x2 . x5 . x0

main : nat
main = 1