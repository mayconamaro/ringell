true : nat
true = 0

false : nat
false = 1

not : nat -> nat
not = \x : nat match x zero -> false suc w -> true

and : nat -> nat -> nat
and = \x : nat \y : nat match x zero -> (match y zero -> true suc z -> false) suc w -> false

equals : nat -> nat -> nat
equals = \x : nat \y : nat match x zero -> (match y zero -> true suc w -> false) suc w -> (match y zero -> false suc z -> equals . w . z)

main : nat
main = and . (equals . 2 . 2) . (not . (equals . 2 . 3))