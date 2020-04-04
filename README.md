# clj-cdt

A Clojure library for parsing C/C++ code

Contains facilities for:

 * Parsing C/C++ with Eclipse CDT library
 * Writing ASTs back to source
 * Identifying Operator Expressions
 * Modifying ASTs (incomplete)

This project was created through my work which is described in our paper at the
Mining Software Repositories 2018 conference:
[Prevalence of Confusing Code in Software Projects - Atoms of Confusion in the Wild](https://atomsofconfusion.com/papers/atom-finder-msr-2018.pdf).

## Namespaces

All of the interesting functions in this project are located in the following
files in the `src/clj_cdt/` directory:

* [`clj-cdt`](src/clj_cdt/clj_cdt.clj) - Generic tools for parsing C/C++ and
  reading/analyzing ASTs
* [`writer-util`](src/clj_cdt/writer_util.clj) - Serialize ASTs to source code,
  or tree view, or debugging output
* [`expr-operator`](src/clj_cdt/expr_operator.clj) - CDT groups all
  unary/binary/ternary operators together (respectively). This module is a
  simple interface for explore which operators are inside those expressions.
* [`modify-ast`](src/clj_cdt/modify_ast.clj) - *incomplete* Provides functions
  to update the AST in memory.

## Using the framework to parse code
 
The first thing you might want to do, is parse some C code. There are three
main functions for doing this, `parse-file`, `parse-source` and
`parse-frag`. Both functions take a `String` as an argument, and return
an
[`IASTNode`](https://dgopstein.github.io/content/cdt/org/eclipse/cdt/core/dom/ast/IASTNode.html).
`parse-file` and `parse-source` both require whole programs, the former
accepting a filename as its argument and the latter a string containing the
full code. `parse-frag` on the other hand can take any (read "many") partial
program. For example:

```clj
(parse-file "gcc/testsuite/c-c++-common/wdate-time.c")  ;; => CPPASTTranslationUnit
(parse-source "int main() { 1 + 1; }")                  ;; => CPPASTTranslationUnit
(parse-frag "1 + 1")                                    ;; => CPPASTBinaryExpression
```
 
After you've parsed some code, you might reasonably want to see what it looks like:
 
```clj
(->> "gcc/testsuite/c-c++-common/wdate-time.c"
      parse-file
      (get-in-tree [2])
      print-tree)
```
                               
Which should output:
           
```
[]  <SimpleDeclaration>                                      {:line 6, :off 238, :len 39}
[0]  <SimpleDeclSpecifier>                                   {:line 6, :off 238, :len 10}
[1]  <ArrayDeclarator>                                       {:line 6, :off 249, :len 27}
[1 0]  <Name>                                                {:line 6, :off 249, :len 9}
[1 1]  <ArrayModifier>                                       {:line 6, :off 258, :len 2}
[1 2]  <EqualsInitializer>                                   {:line 6, :off 261, :len 15}
[1 2 0]  <IdExpression>                                      {:line 6, :off 263, :len 13}
[1 2 0 0]  <Name>                                            {:line 6, :off 263, :len 13}
```
                                                       
Some other useful functions are:

    print-tree     -> Prints a debug view of the tree structure of an AST plus metadata
    write-tree     -> Takes an AST and returns the code that generated it (inverse parsing)
    get-in-tree    -> Digs down into an AST to get at nested children
