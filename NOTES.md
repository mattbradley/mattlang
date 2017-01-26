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
