def fib(n)
  if n <= 2
    1
  else
    fib(n - 2) + fib(n - 1)
  end
end

def fibs(i, n)
  if i <= n
    print i
    print ": "
    puts fib(i)
    fibs(i + 1, n)
  end
end

fibs(1, 20)
