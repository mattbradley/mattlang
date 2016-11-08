# Language Specification

## Variables

Variables are immutable. Really they should be called value labels, since they
just label the value of some type. This language structure is traditionally
called a "variable"; therefore, that's the name we'll go with.

```
# The variable `x` has its type inferred as `Int`
x = 5
```

```
# Variables can also be defined with a type,
# although this probably isn't necessary
x: String = "hello"
```

```
# Variables can be rebound by assigning another value to it
x: String = "hello"
x = 5

# Even though `x` is initially defined as a `String`,
# rebinding it to another value can change its type
```

## Operators

Operators with a higher precedence number are resolved first.

```
Operator Associativity Precedence (Notes)
=  right 0 (assignment)
|| right 1
&& right 2
== left  3
!= left  3
<  left  3
>  left  3
<= left  3
>= left  3
+  left  4
-  left  4
*  left  5
/  left  5
%  left  5
+  unary 6
-  unary 6
!  unary 6
** left  7
.  left  8 (accessor)
```

Operators are implemented as functions. Single argument functions are unary, and
double argument functions are binary.
