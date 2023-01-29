# `camlox` 🐟

An OCaml port of the Java (`jlox`) implementation of Part I of the book

## Build

This project uses [`dune`](https://dune.readthedocs.io/en/stable/index.html) as the OCaml build system.

```
dune build
```

## Execute

To start the `lox` REPL:

```
dune exec lox
```

You should be greeted with:

```
     __         ______     __  __
    /\ \       /\  __ \   /\_\_\_\
    \ \ \____  \ \ \/\ \  \/_/\_\/_
     \ \_____\  \ \_____\   /\_\/\_\
      \/_____/   \/_____/   \/_/\/_/

=== Welcome to the Lox REPL! Type <Ctrl+D> to quit. ===

[1]:
```

To run a specific `lox` file:

```
dune exec lox filename.lox
```
