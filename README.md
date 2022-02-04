# Ringell

Ringell is a prototype compiler from a simple functional language to a strongly normalizing one.
It features an interpreter for both calculi for comparing result values. The compilation process is 
part of my Master's dissertation. It aims to generate terminating code from general recursive functions,
so they can be used in restrict scenarios that will disallow any kind of loop. 
There is a folder `examples` with several programs.

## Install
Haskell Stack is recommended to compile ringell.
Clone this repository and execute
```bash
stack build
```
to compile the project. If you want it available in your ~/.local/bin folder, run
```bash
stack install
```

## Running
There are two modes in Ringell. The following command will execute the code in a file
without worrying about termination. Example `loop.rl` will run untill you cancel it with
Ctrl-C.

```bash
ringell <file>
```

The following command will execute the code in the file ensuring its execution
will terminate, even if it has to abort it. Provide a non-negative integer value
to indicate how many recursive calls each function is allowed to do.

```bash
ringell <file> <integer>
```

## Syntax
Not that beautiful and heavily inspired in Haskell and Simply Typed Lambda Calculus.
Naturals and functions are available as values, which can be used to construct 
arithmetic operations (see example `power.rl`) and boolean algebra (see example `bool.rl`).
Recursive functions are allowed, but mutual recursion is forbidden. A pattern matching over
naturals is available.