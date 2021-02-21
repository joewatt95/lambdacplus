# cs4215_dependent_types
CS4215 project on dependent types

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
```console
% esy
```

If you just want to install the dependencies without building, use
```console
% esy install
```

To build the package, use
```console
% esy build
```

To run the compiled executable:
```console
% esy ./_esy/default/build/default/bin/main.bc
```

To run the compiled js:
```console
% esy ./_esy/default/build/default/bin/main.bc.js
```
