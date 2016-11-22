# Flow-sensitive Type Checking

Type inference:

```
x = 5 # => inferred as Int
x = true # => rebound as Bool

if some_conditional
  x = "hello" => inferred as String inside this if block
end

# x now inferred as (Bool | String) union type since
# it could be either type depending on some_conditional

x = nil # => rebound as Nil

y = # => (Int | String) since either type could be returned from the if expression
  if some_other_conditional
    100
  else
    "hi"
  end

z = # => Int since both branches return an Int
  if foo
    1
  else
    2
  end

a = # => (Int | Nil) since the implicit else branch is nil
  if foo
    5
  end

```
