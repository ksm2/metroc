Metro Language
==============

![Haskell CI](https://github.com/ksm2/metroc/workflows/Haskell%20CI/badge.svg)

**Metro** is a garbage collected, compiled language for the Web and Server.

Example
-------

Run the example in `examples/print.metro` by calling:

    metroc run examples/print.metro


Usage
-----

Run `metroc help` to see usage instructions:

```
Metro Compiler 0.1.0

SYNOPSIS
  metroc COMMAND
  metroc [-h|--help]
  metroc [-v|--version]

COMPILER COMMANDS
  metroc clean      Remove the target directory.
  metroc build      Build the project to WebAssembly.
  metroc run        Run the project main function.
  metroc test       Executes all project tests.

META COMMANDS
  metroc help       Print this help text and exit.
  metroc version    Display the version number and exit.
```
