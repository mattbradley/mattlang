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

## Updating a Record

A couple of ideas:

```
user = {name: "Matt", age: 29}

user = {user with age: 30}
user = {user | age: 30}
user = {user <- age: 30}
```

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

# Type Aliases

Type aliases can be used to create synonyms for existing simple, compound, or generic types. The type alias
behaves the same way as the aliased type. They can be used interchangeably. In fact, any alias used in a
type annotation is simple replaced with the aliased type.

```
typealias Point = (Int, Int)
```

`Point` and `(Int, Int)` are now equivalent types. A value of type `(Int, Int)` can be used anywhere
that expects `Point`. Notice that there is no way to create a value of `Point`, since there is no
literal `Point` construct. Type aliases are simply a way to document or abbreviate other types.

Type aliases can also accept generic parameters:

```
typealias Optional<T> = T | Nil
typealias Error<T> = { message: String, subject: T }
```

When the parameterized alias is used, its type parameters will be replaced with concrete types:

```
fn print_error(error: Error<Int>) -> Nil

# Is equivalent to:

fn print_error(error: { message: String, subject: Int }) -> Nil
```

Type aliases can be nested inside modules and accessed with the dot operator:

```
module Math
  typealias Vector = (Float, Float)
end

fn to_string(vector: Math.Vector) -> String
```

# New Types

The `type` keyword can be used to declare new types. This is similar to `typealias`; however,
the type name and the definition cannot be used interchangably. The new type becomes a nominal
subtype of the type definition, meaning it must be used by name. The new type being a _subtype_
of the original type is important: any function that accepts the original type will also
accept the new type.

```
type User = {name: String, age: Int}

# Use the `User` constructor to create a literal of this type:
user = User{name: "Matt", age: 29}
user.name # => "Matt"

# This function will only accept a `User` type.
# It will _not_ accept the anonymous record type `{name: String, age: Int}`
fn print_name(user: User) -> String
  IO.puts user.name
end

# This function _will_ accept a `User` type, because
# `User` is a subtype of `{ name: String, age: Int }` which
# is a subtype of `{ name: String }`.
fn print_name(user: { name: String }) -> String
  user.name
end
```

You can think of the `type` keyword as creating a constructor function that accepts
some value of the type definition. This would be useful for differentiating between
two nominal types with the same underlying structure:

```
module Math
  type Point = (Float, Float)
  type Complex = (Float, Float)

  # It doesn't make much sense to calculate the geometric distance
  # between two complex numbers. This function will only accept
  # tuples constructed using the Point constructor.
  fn distance(p1: Point, p2: Point) -> Float
    diff = p2 - p1
    sqrt(diff.0 * diff.0 + diff.1 + diff.1)
  end
end
```

Types created with the `type` keyword can be generic types or union types:

```
type Stack<T> = List<T>

module Stack
  fn push<T, U>(stack: Stack<T>, element: U) -> Stack<T | U>
    element :: stack # The `::` operator function accepts `Stack` because `Stack` is a subtype of `List`
  end

  fn pop<T>(stack: Stack<T>) -> (T, Stack<T>)
    top :: rest = stack
    (top, rest)
  end
end
```

# Pattern Matching (Destructuring Assignment)

```
# Pattern match on a 2-tuple to swap values
x = 1
y = 2
(x, y) = (y, x) # => (2, 1)
(x, y) # => (2, 1)

# Pattern match on a record
user = { name: "Matt", age: 29, city: "Columbus" }
{ name: name, age: age } = user # Pull out just a subset of the fields, => { name: "Matt", age: 29, city: "Columbus" } (returns full record)
{ name, age } = user # Pull out a subset by field only, variables are set according to the field name, => return full record also
name # => "Matt"
age # => 29

# Cons pattern matching
head :: tail = [1, 2, 3, 4] # => [1, 2, 3, 4] : List<Int>
head # => 1 : Int
tail # => [2, 3, 4] : Int
```

# Pattern Matching (Case Expression)

```
# Gets the first element of the list, or nil if the list is empty
case [1, 2, 3]
  x::x -> x
  []   -> nil
end

case name
  "Matt" ->
    IO.puts("nick")
    true

  "Matthew" ->
    IO.puts("full")
    true

  _ ->
    IO.puts("none")
    false
end

# Pattern match on a record
case { name: "Matt", age: 29, location: ("Columbus", "OH") }
  { name: "Matt", age: age } ->
    IO.puts(age) # Equivalent to `{ name: "Matt", age: _, location: (_, _) }`; any fields left out are wildcards
  { age: 65, location: (_, "FL") } ->
    IO.puts("retired?") # Equivalent to `{ name: _, age: 65, location: (_, "FL") }`
  _ ->
    IO.puts("don't care")
end
  
```

# Flow Typing

```
x = foo() # => Suppose `foo()` returns a `String | Int`

x # => String | Int

if x :? String # Suppose `:?` is a type-checking operator
  # In this branch, we know that `x :? String` evaluates to true.
  # Therefore, `x` has the type `(String | Int) & String` in this branch.
  # The `&` constructs an intersection type. The new type reduces like so:
  #   (String | Int) & String
  #   => (String & String) | (Int & String)
  #   => (String & String) | Nothing       (`Nothing` is the bottom type)
  #   => String | Nothing
  #   => String
else
  # In this branch, we know that `x :? String` evaluates to false.
  # We can infer that `x` should be an Int, since it can't be a String.
  # The calculated type is `(String | Int) & !String`, which reduces like so:
  #   (String | Int) & !String
  #   => (String & !String) | (Int & !String)
  #   => Nothing | (Int & !String)
  #   => Nothing | Int
  #   => Int
end

x # => Back to String | Int
```
