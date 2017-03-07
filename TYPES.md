fn foo(x: Int) -> Int
fn foo(x: String) -> String

foo : (Int | String) -> (Int | String)
foo : (Int -> Int) & (String -> String)

foo(5) : Int
foo("hi") : String

----

{ a: T1, b: T2 } | { a: T3, c: T4 } = { a: T1 | T3 }

{ a: T1, b: T2 } & { a: T3, c: T4 } = { a: T1 & T3, b: T2, c: T4 }

(T1 -> T2) | (T3 -> T4) = (T1 & T3) -> (T2 | T4)


