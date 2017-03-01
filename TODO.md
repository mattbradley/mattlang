# TODO

[x] parse and ignore comments
[x] pipe operator
[x] repl
[x] modules and requires
[x] tuple
[x] records
[x] type aliases
[ ] newtypes
[ ] macros
[ ] handle ambiguous op
[x] allow lambda literals with no arguments, i.e. { do_something() } instead of { () -> do_something() }
[x] type infer a lambda that is immediately sent to a fn, i.e. reduce([1, 2, 3], { x, acc -> acc + x })
[?] allow destructured union to be called on lambda?, i.e. a call with arg of type Int | Float might dispatch to a lambda for Int but a fn for Float
[x] simple list pattern matching
[x] tuple assignment destructuring
[ ] single expr fn syntax. also, fns without a return type implicitly returning nil
[ ] "value-less" record matching, i.e. `{ name, age } = { name: "Matt", age: 29 }` matches and binds `name` and `age` without explicit variable names
[ ] flow-sensitive typing
[ ] protocols/type classes
[ ] rework the List<T>/EmptyList way of implementing list types? maybe with a bottom `Nothing` type and `typealias EmptyList = List<Nothing>`

# Bugs

[x] fix the bug where `fn foo(x: Int)` isn't properly added to `fn foo<T>(x: T)` when `foo` is called with a parameter type `@U`. `@U` could possibly be `Int`
[ ] fix interpreter bug where assignment in if statement doesn't set variable to nil if branch isn't executed
[ ] fix semantic and interpreter bug where outer scope binding is overwritten by nil if there is no else branch (outer scope meaning a lambda's closure)
[ ] fix semantic bug where fn scope is not isolated from outer scope
[ ] fix ** with negative exponent
[ ] error when requiring a file that doesn't exist
[ ] allow repl to require the same file again if the first require errored
[ ] allow a fn to be replaced in the repl, instead of just appending it (the new fn might never be used if a previous definition always matches)
[ ] allow a typealias to be replaced in repl? maybe only fns and typealiases can only exist inside modules?
[x] during module resolution, don't allow a module path to backtrack more than once, i.e. `Global.Outer.Inner` should not match `Global.Inner`
