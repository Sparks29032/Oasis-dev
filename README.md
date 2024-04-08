# Oasis (formerly CamlTree)
## Scanning and Parsing
Relevant compiler files:
- `ast.ml`: abstract syntax tree (AST) and pretty-print functions
- `scanner.mll`: scanner
- `parser.mly`: parser

To see the result of the scanner and parser on a `.trs` (tree-script) file, build the AST-generation portion of the compiler
```
ocamlbuild genast.native
```
and pipe the `.trs` file into the program
```
cat <file-name>.trs | ./genast.native
```

Example `.trs` programs:
- `prime.trs`: example function demonstrating for loops and list comprehension (the list `[1:n/2-1, n/2:n/2, n/2+1:n]` is parsed as `[1:n/2-1, n/2, n/2+1:n]`)
- `fibonacci.trs`: example (inefficient) computation of a small fibonacci number using trees
