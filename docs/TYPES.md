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

protocol Comparable<T>
  fn compare(a: T, b: T) -> Comparison
end

implement Comparable<Number>
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
