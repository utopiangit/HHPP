
-- guarded recursion

sma :: [Double] -> [Double]
sma (x0:x1:xs) = (x0 + x1) / 2 : sma (x1:xs)
sma xs = xs

-- sma does not call sma recursively until the value is needed
-- before the evaluation, the second argument of (:) is evaluated up to first constructor (WHNF)
-- therefore, sma is not tail-recursive, but don't waste memory
