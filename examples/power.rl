sum : nat -> nat -> nat
sum = \x : nat \y : nat match x zero -> y suc w -> sum . w . suc y

mult : nat -> nat -> nat
mult = \x : nat \y : nat match x zero -> 0 suc w -> sum . y . (mult . w . y)

power : nat -> nat -> nat
power = \x : nat \y : nat match y zero -> 1 suc w -> mult . x . (power . x . w)

main : nat
main = power . 2 . 3
