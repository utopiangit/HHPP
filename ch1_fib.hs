
-- GHC performs aggressive inlining.
-- It's very likely that values and functions get calculated more often than intended.

-- memoized
-- because (map fib [0..] !!) is Constant Applicative Form
fib_mem :: Int -> Integer
fib_mem = (map fib [0..] !!)
  where
    fib 0 = 1
    fib 1 = 1
    fib n = fib_mem (n - 2) + fib_mem (n - 1)

-- not memoized
-- (map fib [0..] !! x) is not CAF because x is free variable
fib_mem_arg :: Int -> Integer
fib_mem_arg x = map fib [0..] !! x
  where
    fib 0 = 1
    fib 1 = 1
    fib n = fib_mem_arg (n - 2) + fib_mem_arg (n - 1)

-- CAF may cause too unintended memoization and space leak.
-- All codes that allocates lots of memory should be wrapped in functions that take one or more parameters.
