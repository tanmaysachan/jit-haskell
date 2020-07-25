# jit-haskell
JIT compilation with haskell and its llvm bindings

* Compile main with ghc to see IR
* `ghc --make Main.hs`

* Compile GetAST to get the AST

requirements - llvm-hs-9.0.0, llvm-config-9

* `cabal install llvm-hs`
