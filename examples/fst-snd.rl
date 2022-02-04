fst : nat -> nat -> nat
fst = \x : nat \y : nat x

snd : nat -> nat -> nat
snd = \x : nat \y : nat y

main : nat
main = snd . (fst . 4 . 5) . (fst . 2 . 3)