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

----

# Nominal Types

type Queue<T> = { front: List<T>, rear: List<T> }

Queue<T> is a subtype of { front: List<T>, rear: List<T> }
but not the other way around.

So `Queue<Int>.subtype?(...)` is only true for another `Queue<Int>` (or subtypes).

type IntQueue = Queue<Int>

`IntQueue` is a subtype of `Queue<Int>`.

Named.subtype?(type)
Named == type || (type.is_a Named && subtype?(type.actual_type)

type.subtype?(Named)
true if type.subtype?(Named.actual_type)

----

type Tree<T> = (T, Tree<T> | Nil, Tree<T> | Nil)

type IntTree = Tree<Int>

type Foo = Bar
type Bar = Foo

type File = String
type OpenFile = File

File : String -> File
OpenFile : File -> OpenFile

----

# Protocols

Protocol rules:
  * The protocol name (with type params) must appear once and only once in each fn
    * This ensures that union types of types that implement the protocol also implements the protocol
    * For instance: `Int : Equatable<Int | Float>` and `Float : Equatable<Int | Float>` imply that `Int | Float : Equatable<Int | Float>`
  * Type params may appear any number of times

```
module Collections
  module BST
    type Node<T: Comparable<T>> = Nil | (T, Node<T>, Node<T>)

  end
end

node = Collections.BST.Node(0, Collections.BST.Node(1.0, nil, nil), nil)

Node<Int | Float : Comparable<Int | Float>>

enum Comparison =
  Smaller,
  Larger,
  Equal

typealias Number = Int | Float

protocol Comparable
  fn compare(a: Comparable, b: Comparable) -> Comparison
end

impl Number : Comparable
  fn compare(a: Number, b: Number) -> Comparison
    diff = a - b

    if diff < 0
      Comparison.Smaller
    elsif diff > 0
      Comparison.Larger
    else
      Comparison.Equal
    end
  end
end
```

```
protocol Enumerable<T>
  fn reduce<U>(e: Enumerable<T>, acc: U, f: (U, T) -> U) -> U
end

impl List<T> : Enumerable<T>
  fn reduce<U>(list: List<T>, acc: U, f: (U, T) -> U) -> U
  end

  fn count(list: List<T>) -> Int
  end

  fn member?(list: List<T>, element: T) -> Bool
  end
end

impl <K, V> Enumerable<(K, V)> for Hash<K, V>
  fn reduce<U>(hash: Hash<K, V>, acc: U, f: (U, (K, V)) -> U) -> U
  end
end

impl <T, U> (T, U) = Equatable

impl Enumerable<T> for List<T>
impl Enumerable<T> for Tree<T>

List<T> | Tree<T> <?< Enumerable<T>

protocol Equatable<T>
  fn equal?(a: Equatable<T>, b: T) -> Bool
end

impl Equatable<Float | Int> for Int
  fn equal?(a: Int, Float | Int) -> Bool
end

impl Equatable<Float | Int> for Float
  fn equal?(a: Float, Float | Int) -> Bool
end

impl Equatable<Float | Int> for Float | Int
  fn equal?(a: Float | Int, b: Float | Int)
end

protocol Equatable
  fn equal?(a: Equatable, b: Equatable)
end

impl Equatable for Int
  fn equal?(a: Int, b: Int)
end

impl Equatable for Float
  fn equal?(a: Float, b: Float)
end

x : Int | Float
y : Int | Float
equal?(x, y)
```

```
Number : Equatable
String : Equatable

but Number|String is not a subtype of Equatable
because this function clause isn't defined:

fn equal?(a: Number, b: String) -> Bool
```

```
protocol Equatable
  fn equal?(a: Equatable, b: Equatable) -> Bool
end

impl Number : Equatable
  fn equal?(a: Number, b: Number) -> Bool
    `@a == @b`: Bool
  end
end

fn all_equal? <T: Equatable> (e1: Enumerable<T>, e2: Enumerable<T>) -> Bool
end
```

```
fn sort(seq: Sequence) -> Sequence
  (xs, ys) = split(seq)
  first(xs) < first(ys)
end
```

----

# Algebraic Data Types / Enums

```
enum Maybe<T>
  Just(T),
  None
end

type Just<T> = T
type None
typealias Maybe<T> = Just<T> | None
```

## Overloading `type` Keyword for ADTs

Different kinds of custom types:
  1. Anonymous compound or complex type
    * Anonymous tuple: `(Int, String)`
    * Anonymous record: `{name: String, age: Int}`
    * Function: `(Float, Float) -> Float`
    * Union or intersection: `(Int, Int) | (Float, Float)`, `Printable & Comparable<Int> | Nil`
  2. Type alias: a synonym for an existing type; useful for abbreviation or documentation
    * `typealias Number = Int | Float`, `typealias Point = (Number, Number)`
    * Does not actually create a fresh type; just expands to the type definition
  3. Named types, nominal types: a fresh type that is a subtype of the type definition and a supertype to only `Nothing`
    * Values of the nominal type are instantiated using the constructor with the same name
    * `type User = {name: String, age: Int}`
      * Creates a new type named `User`. It has no subtypes other than `Nothing` and is itself a subtype of `{name: String, age: Int}`
      * Creates a constructor function named `User` of type `{name: String, age: Int} -> User`
      * `User({name: "Matt", age: 29})` or `User {name: "Matt", age: 29}` or `User{name: "Matt", age: 29}`
    * The type definition can be a union or intersection or any other type
  4. Variant types, algebraic data types, tagged disjoint unions: a union type where each element of the union is its own fresh type and constructor
    * `type Maybe<T> of Just(T) | None`?
    * Are these necessary since we have anonymous union types?

# Intersection Types

```
X & Y <: X
X & Y <: Y
X & Y <: X | Y

X | Y !<: X & Y

A type is a subtype of X & Y if it is a subtype of both X AND Y.
```
