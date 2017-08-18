# Mattlang

Mattlang is a statically-typed, functional toy language inspired by Elixir and Swift. It features a robust type system (tuple and record types, structural and nominal subtyping, parametric polymorphism, lambda types, union and intersection types, protocols, type inference), complex pattern matching (with exhaustiveness and usefulness checking), `requires` and module dependency management, and an interactive REPL.

The code runs in an interpreter, but there is a half-finished code generator that uses C as an intermediate representation in the `codegen` branch.

## Examples

See [`examples/`](./examples) for some example programs.

## Dependencies

The compiler is written in Ruby. The dependencies are managed by Bundler.

Run `gem install bundler` to install Bundler.

Run `bin/setup` to install gems.

## Usage

Run `exe/matt FILENAME` to interpret a file.

Run `exe/matt` to open a REPL to run expressions interactively.
