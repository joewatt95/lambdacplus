# λC+
## Short one line description
λC+ is a proof assistant based on the Calculus of Constructions (CoC).

## What is it really?
At its core, λC+ is an implementation of a dependently typed lambda calculus
based on CoC.
The Curry-Howard correspondence allows us to encode logical connectives and
quantifiers as types in λC+.
This allows us to treat it as a proof assistant.

## Specs
Refer to the `latex` directory.

## Web interface
For an easy to use this language, download the `index.html` and `main.bc.js`
files found in the main project directory. Then open the html file in your
preferred browser. A friendly web-based IDE powered by the ACE editor will be
there to greet you.

## Sample proof
```lean
-- Formalization of Lawvere's fixed point theorem, which captures the essence of
-- the diagonal argument as found in Cantor's famous theorem on cardinality.

constant X : Type
constant Y : Type

-- Definition of equality.
-- Technically it's a type constructor parameterized by X : Type.
constant eq : forall (X : Type) (x : X) (y : X), Prop

-- Reflexivity of =
axiom eq_refl : forall (X : Type) (x : X), eq X x x

-- Symmetry of =
axiom eq_symm : forall (X : Type) (x : X) (y : X), (eq X x y) -> (eq X y x)

-- Transitivity of =
axiom eq_trans : 
    forall (X : Type) (x : X) (y : X) (z : X),
        (eq X x y) -> (eq X y z) -> (eq X x z)

-- If 2 functions are equal f = g, then f x = g x for every x : X.
axiom congr_fun :
  forall (f : X -> Y) (g : X -> Y),
    (eq (X -> Y) f g) -> (forall x : X, eq Y (f x) (g x))

-- f has a fixed point if f x = x for some x : X.
def has_fixed_point :=
  fun (X : Type) (f : X -> X) => exists x : X, eq X (f x) x

-- f is surjective
def surjective :=
  fun (X : Type) (Y : Type) (f : X -> Y) => forall y : Y, exists x : X, eq Y (f x) y

theorem cantor :
(exists g : X -> X -> Y, surjective X (X -> Y) g) -> forall f : Y -> Y, has_fixed_point Y f := 
  assume h (f : Y -> Y),
    -- Existential elimination to pull apart h.
    let {g, this} := h in

    -- Define the diagonal function. This picks out the elements along the diagonal
    -- and flips them around using f.
    let diag : X -> Y := fun x => f (g x x) in

    -- Since g : X -> X -> Y, there must be some x : X with g x = diag
    -- Grab the witness, x, and the proof that g x = diag.
    have exists x : X, eq (X -> Y) (g x) diag, from this diag,
    let {x, this} := this in

    -- Next we have some boring manipulations using the axioms of equality to
    -- establish that (g x x) = f (g x x) and then flip the equality around.

    -- Since (g x) and diag are equal as functions, g x x = diag x
    have h1 : eq Y (g x x) (diag x), from congr_fun (g x) diag this x,

    -- diag x = f (g x x) must hold for this specific x we're working with because
    -- that's the definition of the diagonal function, diag
    have h2 : eq Y (diag x) (f (g x x)), from eq_refl Y (diag x),
    -- By transitivity, g x x = f (g x x)
    have eq Y (g x x) (f (g x x)), from 
      eq_trans Y (g x x) (diag x) (f (g x x)) h1 h2,
    -- By symmetry, f (g x x) = g x x
    have eq Y (f (g x x)) (g x x), from eq_symm Y (g x x) (f (g x x)) this,
 
    -- Use (g x x) and h to witness the existentially quantified statement that f
    -- has a fixed point.
    show has_fixed_point Y f, from {g x x, this}
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
