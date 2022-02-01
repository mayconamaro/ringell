# Ringell

Ringell is a prototype compiler from a simple functional language to a strongly normalizing one.
There is a file `example.rl` in the examples folder. When executing `ringell` on it, all steps should execute succesfully and exit showing the AST of the unrolled program. At the moment, I can't get evaluation to work properly.

Here's a hint how to run ringell. The number 5 is how many times we want each recursive function to be unrolled.

```bash
ringell example.rl 5
```
