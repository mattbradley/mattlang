# TODO

[x] parse and ignore comments
[x] pipe operator
[x] repl
[x] modules and requires
[x] tuple
[ ] named tuples
[ ] type aliases, named subtypes
[ ] macros
[ ] handle ambiguous op
[x] allow lambda literals with no arguments, i.e. { do_something() } instead of { () -> do_something() }
[x] type infer a lambda that is immediately sent to a fn, i.e. reduce([1, 2, 3], { x, acc -> acc + x })
[ ] allow destructured union to be called on lambda?, i.e. a call with arg of type Int | Float might dispatch to a lambda for Int but a fn for Float
[x] fix the bug where `fn foo(x: Int)` isn't properly added to `fn foo<T>(x: T)` when `foo` is called with a parameter type `@U`. `@U` could possibly be `Int`
[ ] simple list pattern matching
[ ] tuple assignment destructuring
[ ] single expr fn syntax. also, fns without a return type implicitly returning nil
[ ] fix interpreter bug where assignment in if statement doesn't set variable to nil if branch isn't executed
[ ] fix semantic and interpreter bug where outer scope binding is overwritten by nil if there is no else branch (outer scope meaning a lambda's closure)
[ ] fix semantic bug where fn scope is not isolated from outer scope
