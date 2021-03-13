# CS4215 dependent types
## Overview
CS4215 project on dependent types.
This is a strongly normalizing lambda calculus with dependent types.
The main focus is to implement the dependent Pi type, which generalizes the
simple function type.

### Specs
Refer to the `latex` directory.

## New features
### 2021-03-14
- Implement optional type annotations for input arguments to functions
  - For instance, we can now write `fun (T : Type) (x : T) => x`.
  - The semantics of this have been documented in the specs.

## Current status
### What seems to work
- Normalization
- Context
- Type checking and inference

### What needs working on
- Practical side
  - More testing
  - Better error reporting and error messages
    - At the moment we simply `assert false` whenever we encounter an error
  - Better pretty printing
  - Glue code for tying everything together
  - REPL environment
  - Tie our code into Typescript frontend

- Theory side
  - What theoretical properties does our language possess?
    - ie is type checking decidable? will our bidirectional algorithm always terminate?
  - Add Sigma types, binary product, binary coproduct
  - Stratify universe into 2 levels, like 2 sorted set theories, to avoid Girard's paradox, ie Burali-Forti.
  - Local let bindings would be nice too.

## Project structure
- `bin` is where all the driver code for the compiled
executable can be found. Currently it only contains `main.ml`, which we are
using for testing and experimentation.
- `lib` is our library which contains all the code making our language work.
    - `parsing` contains all the functions we use for parsing.
        - `ast.ml` contains the AST generated by the parser. This uses strings
        to identify variables.
        - `lexer.ml` contains our lexer.
        - `grammar.mly` contains our grammar, written using Menhir.
        - `parser.ml` contains some functions and boilerplate code tying the
        lexer and grammar together. This provides an interface which we use to
        parse our language.
        - `location.ml` contains a `located` datatype and other stuff which we use
        to decorate our ASTs as found in `lib/parsing/ast.ml` and 
        `lib/kernel/ast.ml` with source locations. This info will be used for
        error reporting.
    - `kernel` contains all the important stuff, like normalization and type
    checking. Basically everything that happens after parsing can be found here.
        - `ast.ml` contains our internal AST. Unlike the one in `lib/parsing`,
        this uses de bruijn indices for variables, rather than strings.
        - `context.ml` is the module implementing the context/environment.
        The level of abstraction is controlled via the module signature found in
        `context.mli`.
        This functions like a list of triples of triples of the form
                (var name string, type of var, binding)
        - `normalization.ml` currently contains the `shift` and `subst`
        functions for working with our AST using de bruijn indices.
        We aim to implement the `normalize` function for expressions soon.
        - `typing.ml` is currently empty. Eventually we will implement our
        bidirectional typechecking algorithm here.
        This will include the 2 key functions `check` and `infer`, where `infer`
        involves synthesizing the type, while `check` involves verifying if an
        expression has a given type.
        - `debruijn.ml` contains facilities for converting the parser's AST to 
        the internal AST and vice versa. For an overview of the key functions,
        refer to its module signature, `debruijn.mli`.
        - `statements.ml` contains functions for evaluating statements.
    - `utils` contains utilities used in all parts of the project.

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
$ esy ./_esy/default/build/default/bin/main.bc.js
```

To ease testing, the `run_main.sh` script has been provided with the command
`esy ./_esy/default/build/default/bin/main.bc`.
