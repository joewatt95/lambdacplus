# λC+
A small proof assistant based on the Calculus of Constructions (CoC).

## Description
At its core, λC+ is an implementation of a dependently typed lambda calculus
based on CoC.
Through the Curry-Howard correspondence, logical connectives and quantifiers are
represented by types in λC+.
We can then express theorems using these types and provide proofs.

For instance, here's an example formalizing the reflexivity of implication
```lean
theorem A_implies_A :
-- This says that for any proposition A, A implies A itself.
forall (A : Type), A -> A :=
-- A proof of this proposition is a function which takes a proposition A and
-- a proof of `A` and then returns a proof of `A`.
  fun (A : Type) (a : A) => a
```
Notice that the proof of this theorem is the polymorphic identity function.

Syntactically, λC+ looks and feels a lot like the 
[Lean theorem prover](https://leanprover.github.io/), which in turn resembles
Coq and Ocaml.

In fact, we also support a subset of Lean's syntactic sugar for writing
[structured proof terms](https://leanprover.github.io/reference/expressions.html#structured-proofs).

Using this, the above proof may be rewritten as
```lean
theorem A_implies_A :
-- This says that for any proposition A, A implies A itself.
forall (A : Type), A -> A :=
-- Assume that `A` is a type (which we can think of as a proposition), and that
-- `a` is a proof of `A`.
  assume (A : Type) (a : A),
    -- We may conclude `A` because `a` is a proof of it.
    show A, from a
```

Also, one may even write `Prop` instead of `Type` to make it more explicit that
we are to think of `A` as a proposition rather than a data type.

We hope that this helps improve the readability of proofs for those who are more
used to traditional pen and paper proofs as opposed to Curry-Howard style proofs.

## For developers
This section is heavily based on [this example](https://github.com/esy-ocaml/hello-ocaml).

First you need to install [Esy](https://esy.sh/en/) using your distro's native
package manager or via
```console
% npm install -g esy
```

Esy is a package manager for Ocaml and Reason. It helps manage project
dependencies via `package.json`.

To install all the required dependencies and build the project with a single
command, you may run
```shell
$ esy
```

If you just want to install the dependencies without building, use
```shell
$ esy install
```

To build the package, use
```shell
$ esy build
```

To run the compiled executable:
```shell
$ esy ./_esy/default/build/default/bin/main.bc
```

To run the compiled js:
```shell
$ node ./_esy/default/build/default/bin/main.bc.js
```

To ease testing, the `run_main.sh` script has been provided with the command
`esy ./_esy/default/build/default/bin/main.bc`.

## Implemented types
- Pi types for universal quantification
- Sigma types for
  - subtype
  - conjunction
- Existential type for existential quantifier
- Sum types (aka coproduct or tagged union) for disjunction

## Project structure (Work in progress)
### Overview of what happens when you enter a program
Programs entered by the user are first parsed by the parser (found in `lib/parsing`).

### Detailed breakdown of directory structure
- `bin` currently contains only `main.ml` which contains the main entry point to
  interact with our language.
- `lib` is our library which contains all the code making our language work.
    - `parsing` contains all the functions we use for parsing.
        - `lexer.ml` contains our lexer.
        - `grammar.mly` contains our grammar, written using Menhir.
        - `parser.ml` contains some functions and boilerplate code tying the
        lexer and grammar together. This provides an interface which we use to
        parse our language.
    - `kernel` contains all the important stuff for evaluating expressions and
    statements. These include normalization, type checking and context management
    using de bruijn indices.
        - `context.ml` is the module implementing the context/environment.
        - `normalization.ml` currently contains the `normalize`, `subst` and
        `beta_reduce` functions. The latter 2 implement the substitution operation
        for de bruijn ASTs.
        - `typing.ml` implements our bidirectional typechecking algorithm.
        This indluces the 2 key functions `check` and `infer`, where `infer`
        involves synthesizing the type, while `check` involves verifying if an
        expression has a given type.
        - `eval_statements.ml` contains functions for evaluating statements.
    - `common` contains code that is shared between both `kernel` and `parsing`.
       - `location.ml` contains a `located` datatype and other stuff which we use to decorate our AST in `ast.ml`
       with source locations. This info will be used for error reporting.
       - `ast.ml` contains our AST. It is parameterized over a type variable, `'a`, where `'a` can either be `string` or `int`. This type variable denotes the type of variable identifiers. The AST which our parser parses the concrete syntax into uses `string` to identify variables, while our internal one, used for typechecking and normalization, uses `int` denoting de bruijn indices to identify variables.
   - `ast_conv.ml` contains facilities for converting the parser's AST to 
   the internal AST and vice versa.
   - `error_reporting.ml` contains functions for handling errors that occur while
   running programs in our language. These include pretty printing of errors.
   - `pretty_printing.ml` contains utilities for unparsing expressions and pretty
   printing them.
