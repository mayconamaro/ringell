sum : nat -> nat -> nat
sum = \x : nat \y : nat match x zero -> y suc w -> sum . w . suc y

mult : nat -> nat -> nat
mult = \x : nat \y : nat match x zero -> 0 suc w -> sum . y . (mult . w . y)

main : nat
main = mult . 2 . 3
