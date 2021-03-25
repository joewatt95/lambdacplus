# CS4215 dependent types
## Overview
This is a dependently typed lambda calculus based on the pure type system (PTS)
that is the Calculus of Constructions (CC).
Like all the other PTSes in Barendregt's lambda cube, CC is strongly normalizing
and serves as a consistent foundation for theorem proving in higher order
intuitionistic logic.

### Specs
Refer to the `latex` directory.

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

## Sample execution
```shell
$ ./run_main.sh 
Enter input:
// Assume that A and B are types.
constant A : Type
constant B : Type

// Assume that R is a binary relation on A x B.
// Technically speaking, R is a predicate symbol and these are
// represented by type constructors in the Curry Howard interpretation of
// logic.
constant R : A -> B -> Prop

// Further assume that every (a : A) is related to some (b : B).
axiom R_left_total : ∀ a : A, ∃ b : B, R a b

// We define a choice function using the explicit witness provided by the
// constructive existential quantifier.
def f := λ (a : A) =>
  let exists_b_Rab := R_left_total a in
  fst exists_b_Rab

// pf is a proof that given an arbitrary (a : A), a is really related to (f a). 
def pf := λ (a : A) => snd (R_left_total a)

// Use f and pf to witness the existential below.
check ((f, pf) : ∃ f : A -> B, ∀ a : A, R a (f a))

Here's the output:
(Σ (f' : (∏ (_ : A), B)), (∏ (a : A), ((R a) (f' a))))

```

More sample programs can be found in the `sample_programs` directory.

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
        The level of abstraction is controlled via the module signature found in
        `context.mli`.
        Contexts function like a list of triples of triples of the form
                (var name string, type of var, binding)
        - `normalization.ml` currently contains the `shift` and `subst`
        functions for working with our AST using de bruijn indices.
        We aim to implement the `normalize` function for expressions soon.
        - `typing.ml` is currently empty. Eventually we will implement our
        bidirectional typechecking algorithm here.
        This will include the 2 key functions `check` and `infer`, where `infer`
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