# TODO

[x] parse and ignore comments
[x] pipe operator
[ ] repl
[ ] modules and requires
[ ] macros
[ ] handle ambiguous op
[ ] allow lambda literals with no arguments, i.e. { do_something() } instead of { () -> do_something() }
[ ] type infer a lambda that is immediately sent to a fn, i.e. reduce([1, 2, 3], { x, acc -> acc + x })
[ ] allow destructured union to be called on lambda?, i.e. a call with arg of type Int | Float might dispatch to a lambda for Int but a fn for Float
[ ] fix the bug where `fn foo(x: Int)` isn't properly added to `fn foo<T>(x: T)` when `foo` is called with a parameter type `@U`. `@U` could possibly be `Int`
