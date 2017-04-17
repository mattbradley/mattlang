# TODO

[ ] come up with a better name
[x] parse and ignore comments
[x] pipe operator
[x] repl
[x] modules and requires
[x] tuples
[x] records
[x] type aliases
[x] newtypes
[ ] macros
[ ] handle ambiguous op
[x] allow lambda literals with no arguments, i.e. { do_something() } instead of { () -> do_something() }
[x] type infer a lambda that is immediately sent to a fn, i.e. reduce([1, 2, 3], { x, acc -> acc + x })
[?] allow destructured union to be called on lambda?, i.e. a call with arg of type Int | Float might dispatch to a lambda for Int but a fn for Float
[x] simple list pattern matching
[x] tuple assignment destructuring
[ ] single expr fn syntax. also, fns without a return type implicitly returning nil
[ ] "value-less" record matching (field punning), i.e. `{ name, age } = { name: "Matt", age: 29 }` matches and binds `name` and `age` without explicit variable names
[ ] flow-sensitive typing
[x] protocols/type classes
[x] rework the List<T>/EmptyList way of implementing list types? maybe with a bottom `Nothing` type and `typealias EmptyList = List<Nothing>`
[x] `case` expression and exhaustive and useless clause checking for pattern matching
[x] smart type elimination for bindings in `case` patterns
[ ] enums/disjoint sum types
[x] syntax sugar for list types, `[Int]` instead of `List<Int>`
[x] range type
[ ] sigils
[ ] regexes
[x] functional record update
[ ] default values for fn parameters
[x] tail call elimination
[ ] better multiple dispatch
[x] intersection types
[ ] module constants
[ ] currying or partial application?
[ ] require parens on 0-arity functions? https://groups.google.com/forum/#!topic/elixir-lang-core/Otz0uuML764rework
[ ] allow fn calls to accept tuples instead of parenthesized args?
[ ] defining a type that is the same name as a module
[ ] string interpolation

# Bugs

[x] fix the bug where `fn foo(x: Int)` isn't properly added to `fn foo<T>(x: T)` when `foo` is called with a parameter type `@U`. `@U` could possibly be `Int`
[x] fix interpreter bug where assignment in if statement doesn't set variable to nil if branch isn't executed
[x] fix semantic and interpreter bug where outer scope binding is overwritten by nil if there is no else branch (outer scope meaning a lambda's closure)
[x] fix semantic bug where fn scope is not isolated from outer scope
[x] fix ** with negative exponent
[ ] error when requiring a file that doesn't exist
[ ] allow repl to require the same file again if the first require errored
[ ] allow a fn to be replaced in the repl, instead of just appending it (the new fn might never be used if a previous definition always matches)
[ ] allow a typealias to be replaced in repl? maybe fns and typealiases can only exist inside modules?
[ ] in repl, figure out some way to remove fn and module definitions from the global scope when a block entered in the repl fails compilation
[x] during module resolution, don't allow a module path to backtrack more than once, i.e. `Global.Outer.Inner` should not match `Global.Inner`
[ ] assignment of a variable with a module path shouldn't work, i.e. `MyMod.x = 5` should fail
[x] when passing an untyped lambda to an `Anything` arg, infer the type as `(Nothing[, Nothing ...]) -> Anything`
[ ] negative number literals in case patterns
[x] fns should begin with a lowercase letter
[ ] pop out of frames and scopes when an error happens in repl
