# BScript

BScript Language Toolkit

Used to experiment with AST for languages

## Static Typing

All the patterns follow the same general three-pass strategy. 

1. In the first pass, the Parser builds an AST.
2. In the second pass, a tree walker builds a scope tree and populates a symbol table.
3. In the third pass over the AST the Parser computes the type of each expression.

## Resources

- Book [Language Implementation Patterns](https://pragprog.com/titles/tpdsl/language-implementation-patterns/)
