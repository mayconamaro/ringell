false : nat
false = 0

true : nat
true = 1

not : nat -> nat
not = \x : nat match x zero -> true suc w -> false

and : nat -> nat -> nat
and = \x : nat \y : nat match x zero -> false suc w -> (match y zero -> false suc z -> true)

equals : nat -> nat -> nat
equals = \x : nat \y : nat match x zero -> (match y zero -> true suc w -> false) suc w -> (match y zero -> false suc z -> equals . w . z)

main : nat
main = and . (equals . 2 . 2) . (not . (equals . 2 . 3))