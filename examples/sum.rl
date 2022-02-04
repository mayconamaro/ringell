sum : nat -> nat -> nat
sum = \x : nat \y : nat match x zero -> y suc w -> sum . w . suc y

main : nat 
main = sum . 3 . 4