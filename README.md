# cs4215_dependent types
## Overview
CS4215 project on dependent types.
This is a strongly normalizing lambda calculus with dependent types.
The main focus is to implement the dependent Pi type, which generalizes the
simple function type.

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
        - `context.ml` contains a module signature and a concrete module
        implementing the context/environment.
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
        - `debruijn.ml` is deprecated and will be rewritten soon. It currently
        contains facilities for converting the parser's AST to the internal AST
        and vice versa.
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

## Overview of this branch
This branch is for testing an implementation of de Bruijn indices.

Following Spartan tt, we now have 2 layers of AST, namely:
1. the parser's AST, in lib/syntax/parsing/ast.ml
2. the internal AST, in lib/syntax/internal/ast.ml

The former is generated by the parser, and uses strings for variable names,
while the latter uses de Bruijn indices for variables. Note that the AST at this
point is still untyped, ie it hasn't been type checked yet.

The `debruijn.ml` module defines a naming context datatype and an API for
manipulating it. This datatype is used for book-keeping purposes, when converting
variable names to indices and vice versa.

Currently the naming context only handles conversion between de brujin indices
and variable names. It doesn't carry any information about the type or binding
of the variable. This is in line with Spartan tt's approach, which uses separate
contexts and phases for type checking and printing.

The module also contains functions for converting between both ASTs. Most
prominent are:
1. `parser_to_internal_expr`
2. `parser_to_intenal_stmt` 
3. `parser_to_intenal_stmts` 
4. `internal_to_parser_expr`

The first 3 are useful for converting from the parser's AST to our internal one,
while the fourth converts the other way around for printing purposes.
Note that (2) and (3) not only return the converted statement(s) but also the
new context resulting from processing a new variable.

The implementation for these functions is heavily inspired by Spartan tt and
the `Types and Programming Languages` book.

### Testing
To test these functions, `main.ml` has been updated with boilerplate for testing
and a handy `run_main.sh` script has been provided to run the built binary.

A sample run is as follows:
Note that pretty printing for source locations, given by the `source_loc` field
has yet to be implemented, so those appear blank here.
```shell
$ ./run_main.sh
Enter input for parsing:
axiom T : Type
def f := (fun x => x : Pi (x : T), T)

Parser AST:
[{ Location.data =
   Ast.Axiom {var_name = "T";
     var_type = { Location.data = Ast.Type; source_loc =  }};
   source_loc =  };
  { Location.data =
    Ast.Def {var_name = "f";
      var_expr =
      { Location.data =
        Ast.Ascription {
          expr =
          { Location.data =
            Ast.Fun {input_var = "x";
              body = { Location.data = (Ast.Var "x"); source_loc =  }};
            source_loc =  };
          expr_type =
          { Location.data =
            Ast.Pi {input_var = "x";
              input_type = { Location.data = (Ast.Var "T"); source_loc =  };
              output_type = { Location.data = (Ast.Var "T"); source_loc =  }};
            source_loc =  }};
        source_loc =  }};
    source_loc =  }
  ]

De Bruijn AST:
[{ Location.data =
   Ast.Axiom {var_name = "T";
     var_type = { Location.data = Ast.Type; source_loc =  }};
   source_loc =  };
  { Location.data =
    Ast.Def {var_name = "f";
      var_expr =
      { Location.data =
        Ast.Ascription {
          expr =
          { Location.data =
            Ast.Fun {input_var = "x";
              body = { Location.data = (Ast.Var 0); source_loc =  }};
            source_loc =  };
          expr_type =
          { Location.data =
            Ast.Pi {input_var = "x";
              input_type = { Location.data = (Ast.Var 1); source_loc =  };
              output_type = { Location.data = (Ast.Var 2); source_loc =  }};
            source_loc =  }};
        source_loc =  }};
    source_loc =  }
  ]

Final naming context:
[f; T]

Parsing last expr back to parser's AST:
{ Location.data =
  Ast.Ascription {
    expr =
    { Location.data =
      Ast.Fun {input_var = "x";
        body = { Location.data = (Ast.Var "x"); source_loc =  }};
      source_loc =  };
    expr_type =
    { Location.data =
      Ast.Pi {input_var = "x";
        input_type = { Location.data = (Ast.Var "T"); source_loc =  };
        output_type = { Location.data = (Ast.Var "T"); source_loc =  }};
      source_loc =  }};
  source_loc =  }

```

## Future directions
We have 2 ways we can proceed from here.

### Separate printing and typing + binding contexts
The first is to follow Spartan tt and implement separate phases and contexts for
printing and type checking. In this approach, we would have a naming context
and another context carrying binding and typing information.
At the top level, we first construct 2 such contexts, both of which are initially
empty. Whenever a user enters a statement, we do the following:
- Convert the input to our internal AST.
- Next we ignore the naming context and proceed to use the typing + binding context
and step through the statement to typecheck it.

This means that whenever the user enters a statement, we will get a new naming
context and typing + binding context, which we carry along to process the next
statement that is entered.

### Same printing and typing + binding contexts
The second approach is the one is adopted by the `Types and Programming Languages`
book.

TODO: Research more on this approach and compare the differences between both.
