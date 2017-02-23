# Notes

 * Rewrite && and || as short-circuit macros if macros are ever implemented

## Typing

fn +(a: Int, b: Int) -> Int
fn +(a: Float, b: Int) -> Float
fn +(a: Int | Float, b: Float) -> Float

fn foo(l: List<Int | Float>) -> Int

A function arg will match the specified type or any subtypes:
 * T is a subtype of T | U
   * fn foo(a: Int | Float) -- Int will match arg `a`
 * T | U is a subtype of T | U | V
   * fn foo(a: Int | Float | Nil) -- Int | Nil will match arg `a`
 * T<U> is subtype of T<U | V> (assuming data structures are immutable)
   * fn foo(l: List<Int | Float>) -- List<Int> will match arg `l`
   * Generics are covariant, i.e. a Stack<String> is also a Stack<String | Nil>
 * T<U | V> is NOT a subtype of T<U>
   * fn foo(l: List<Int>) -- List<Int | Float> will match NOT arg `l`, since `foo` expects every element in the list to behave like an Int

What's the difference between these two:
  * List<Int> | List<Float>
  * List<Int | Float>
  * It seems they are the same.
      * Retraction: they are not exactly the same; however, each type of the union `List<Int> | List<Float>`
        is a subtype of `List<Int | Float>`. Therefore `List<Int> | List<Float>` is a subtype of `List<Int | Float>`.

Dict<String, Int | Float>
Dict<String, Int> | Dict<String, Float>

List<List<Int | Float>> =>= List<List<Int> | List<Float>> =>= List<List<Int>> | List<List<Float>>

### Special Types

 * Unit type: the unit type is `Nil`. It has exactly one value: `nil`.
    * This is similar to `nil` in Ruby, Elixir, etc.
    * This combines Swift's `nil` and `()` types into a single unit type, since our language doesn't have optionals (union types instead).
    * An empty tuple `()` is also `nil`.
 * Bottom type: this type is the subtype of all other types. No use for this yet, but maybe in the future generics might need it, i.e. `Hash<String, Nothing>`.
 * Top type: something like `Any`, a supertype for all other types. Maybe we'll need this in the future.
 * Empty list type: `EmptyList` has one value `[]`. This is a unit type as well.
    * It might be a good idea to realize isomorphism between `[]` and `nil`, since they are both unit types.
        * This would turn things like `1 :: 2 :: 3 :: []` into `1 :: 2 :: 3 :: nil`.
        * But it might make things like `List.length([]) == 0` look weird: `List.length(nil) == 0`.
        * Other things that might look weird:
            * `[1, 2, 3] ++ []` -> `[1, 2, 3]`, `[1, 2, 3] ++ nil` -> `[1, 2, 3]`
            * `List.map(nil, { ... })` -> `nil` or `[]` (same thing)
            * `List.reverse(nil)` -> `nil`
    * Does having this type mean that we also need `EmptySet`, `EmptyMap`, etc. types? Maybe all those should be `nil` as well.

## Generic Functions

fn foo<T>(a: T, b: T) -> T

a = # Float | Int
  if true
    1
  else
    2.0
  end

b = # Float | Int
  if true
    1.0
  else
    2
  end

foo(a, b) # T: Float | Int

fn == <T>(a: T, b: T) -> Bool
end

Dict<Int | String, Float | Nil> =>= Dict<String, Float>

# Lambda Types

(Int, Int) -> Int

Need to disambiguate functions that return functions:

Int -> Int -> Int means Int -> (Int -> Int), takes an Int and returns an Int -> Int function
 * -> is right associative

Int -> Int, Float -> Float -> Int
 * ((Int -> Int), (Float -> Float)) -> Int
 * (Int) -> (Int, Float) -> Float -> Int
 * This syntax is invalid. Multi-arg lambdas must have the args surrounded in parentheses:
   * (Int -> Int, Float -> Float) -> Int

Need to disambiguate functions and union types:

Int | Float -> Int | Nil, what does this mean?
 * (Int | Float) -> (Int | Nil), ***this one***
 * Int | (Float -> Int) | Nil
 * | has higher precedence than ->

# Lambdas

Return type is inferred. This is okay because anonymous functions don't usually need to be documented.
Argument type annotations can be left out if they can be inferred (by passing the function to another function
that does have type annotations).

Args must be wrapped in parentheses to disambiguate between function types as args.
 * { f: Int -> Int -> f(5) } # This would be hard to parse without backtracking
 * { (f: Int -> Int) -> f(5) } # Easier to parse and much clearer what type `f` is

fn map<T, U>(list: List<T>, transform: T -> U) -> List<U>

f = { (x: Int | Float, y: Int | Float) -> x + y }
f = { (x, y) -> x + y }
f = { (g: (Int | Float) -> Int) -> g(0) }

Shorter lambda syntax for when the args can be inferred by immediately sending the lambda to a fn:

List.map([1, 2, 3]) { x -> x * x }

# Strings

Maybe implemented as lists of unicode scalars (32bit ints), with helper functions for
converting to a list of graphemes, bytes, codepoints etc. Equality would check for
grapheme equality.

# Records (Named Tuples)

book = { title: "Moby Dick", author: "Herman Melville" }

## Record Typing

A record A is a subtype of a record B if A has all of B's labels, and the types of each of A's labels
is a subtype of the corresponding label in B.

For instance:

```
{ name: String, age: Int, city: String } <=< { name: String, age: Int }
```

The first record is a subtype of the second since it contains all of the second's labels
and the same types for those labels. Note that even though it has an extra `city` label,
it is still a subtype, since every function that accepts the second type will expect it
only to have `name` and `age` labels of type `String` and `Int`, which the first type
satisfies.

Another example:

```
{ name: String, age: Int, friends: List<String> } <=< { name: String, age: Int, friends: List<String | Nil> }
```

The first record type is a subtype of the second, since it has all the same labels
and each label's type is a subtype of the corresponding label in the second record's type.
`List<String>` is a subtype of `List<String | Nil>`.

# Single-expression Function Return Type Inference

A special fn definition syntax can be used for fns with only a single expression. The return type is inferred in this case.

```
# The return type for this function is inferred as Float.
fn pyth(a: Float, b: Float) = Math.sqrt(a * a + b * b)

# This is the equivalent fn definition.
fn pyth(a: Float, b: Float) -> Float
  Math.sqrt(a * a + b * b)
end
```
