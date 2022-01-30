# Ringell

## Example Program

You can create a file `example.rl` with the code below. When executing `ringell` on it, all steps should execute succesfully and exit without printing any message. At the moment, only parsing and typecheking are done.

```
sum : nat -> nat -> nat
sum = \x : nat \y : nat (match x zero -> y suc w -> sum . w . suc y)

mult : nat -> nat -> nat
mult = \x : nat \y : nat (match x zero -> 0 suc w -> sum . y . (mult . w . y))

main : nat
main = mult . 3 . 4
```
