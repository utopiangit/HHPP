
mySum ::(Num a) => [a] -> a
mySum [] = 0
mySum (a:as) = a + mySum as

mySum2 s [] = s
mySum2 s (x:xs) = let s' = s + x in mySum2 s' xs

mySum2' s [] = s
mySum2' s (x:xs) = let s' = s + x in s' `seq` mySum2' s' xs 
