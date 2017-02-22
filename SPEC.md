TODO: replace Mattlang with actual language name when one is chosen.

# Mattlang

## Language Specification

### Basic Types and Operators

Mattlang has support for several built-in types: integers, float, booleans, and
strings. Collection types (lists and maps) and compound types (tuples and
structs) are introduced later.

```
# Basic literals (the # mark starts a comment, which is ignored by the compiler)

10      # Int
10.0    # Float
true    # Bool
"hello" # String
```

Arithmetic operators are performed using standard infix operators like `+`, `-`,
`*`, `/`, etc. The precedence for each operator determines the order that the
operators in an expression are evaluated. For instance, `1 + 2 * 3` evaluates to
`7`; the multiplication is performed before the addition because it has a higher
precendence. Here are the built-in arithmetic operators:

| | | | |
| --- | --- | --- | --- | --- |
| `+` | 6 | `1 + 2` => `3` | |
| `-` | 6 | `5 - 3.5` => `1.5` | |
| `*` | 7 | `2.5 * 3` => `7.5` | |
| `/` | 7 | `12 / 5` => `2.4` | |
| `//` | 7 | `12 // 5` => `2` | integer division |
| `%` | 7 | `12 % 5` => `2` | modulo |
| `**` | 8 | `2 ** 3` => `8` | exponentiation (right-associative) |

You can wrap expressions in parentheses to override operator precedence:
`(1 + 2) * 3` gives `9`.

Most infix operators are left-associative (meaning subsequent operators of the
same precedence are evaluated left-to-right); however, some are
right-associative, like the exponentiation operator `**`. This means that the
expression `2 ** 3 ** 2` is evaluated like `2 ** (3 ** 2)` giving `512`.

You cannot change the precedence or associativity of operators that have already
been defined, but you can create your own custom operators. See {TODO SECTION}
for more information.

### Collection Types

The basic collection type is a (linked) list. Use square brackets `[]` to
create a list literal.

```
[1, 2, 3] # A linked list with three elements
```
