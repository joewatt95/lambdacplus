# CS4215 dependent types
## Overview
This is a dependently typed lambda calculus based on the pure type system (PTS)
that is the Calculus of Constructions (CC).
Like all the other PTSes in Barendregt's lambda cube, CC is strongly normalizing
and serves as a consistent foundation for theorem proving in higher order
intuitionistic logic.

### Specs
Refer to the `latex` directory.

## Sample proof
```lean
constant A : Type
constant B : Type

constant R : A -> B -> Prop

-- The axiom of choice is provable constructively due to the strong elimination rule
-- of the Sigma type.
theorem choice : 
(forall a : A, exists b : B, R a b) -> exists f : A -> B, forall a : A, R a (f a) := 
  assume R_left_total,
    -- Define the magic choice function :^)
    -- This looks into the proof that R is left total and grabs an explicit
    -- witness for a.
    let f := fun (a : A) =>
      have exists b : B, R a b, from R_left_total a,
      fst this
    in
    have forall a : A, R a (f a), from
      assume a,
        have exists b : B, R a b, from R_left_total a,
        show R a (f a), from snd this,
    show exists f : A -> B, forall a : A, R a (f a), from (f, this)

check choice
```

More sample programs can be found in the `sample_programs` directory.

## Usage
This section is heavily based on [this example](https://github.com/esy-ocaml/hello-ocaml).

First you need to install [Esy](https://esy.sh/en/) using your distro's native
package manager or via:
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

## Current status
### Implemented types
- Pi types for universal quantification
- Sigma types for
  - existential quantifier
  - subtype
  - conjunction
- Sum types (aka coproduct or tagged union) for disjunction

### What needs working on
  - [ ] More testing
  - [x] Better error reporting and error messages
  - [x] Better pretty printing
  - [ ] Glue code for tying everything together
  - [ ] REPL environment
  - [ ] Tie our code into Typescript frontend
  - [ ] Unit, aka the singleton type, aka logical truth
  - [ ] Void, aka the empty type, aka logical falsity

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