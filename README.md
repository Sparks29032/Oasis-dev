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
- `prime.trs`
- `fibonacci.trs`
