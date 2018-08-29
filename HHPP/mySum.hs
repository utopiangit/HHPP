
-- This recursion builds up thunks.
mySum ::(Num a) => [a] -> a
mySum [] = 0
mySum (a:as) = a + mySum as

-- This is tail recursion, but finally builds up thunks.
mySum2 s [] = s
mySum2 s (x:xs) = let s' = s + x in mySum2 s' xs

-- Prevent thunks by evaluating strictly.
mySum2' s [] = s
mySum2' s (x:xs) = let s' = s + x in s' `seq` mySum2' s' xs
