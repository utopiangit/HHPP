-- Chapter 1 minfree
import Data.List
import Data.Array
import Control.Monad.ST
import Data.Array.ST

minfree xs = head $ [0..] \\ xs
minfree' = search . checklist

minfree'' xs = minfrom 0 (length xs, xs)
minfrom a (n, xs)
  | n == 0 = a
  | m == b - a = minfrom b (n - m, vs)
  | otherwise = minfrom a (m, us)
  where
    (us, vs) = partition (<b) xs
    b = a + 1 + n `div` 2
    m = length us
                  

search :: Array Int Bool -> Int
search = length . takeWhile id . elems

checklist xs = runSTArray (do
    a <- newArray (0, n) False
    sequence [writeArray a x True | x <- xs, x <= n]
    return a)
  where n = length xs

{-
checklist :: [Int] -> Array Int Bool
checklist xs = accumArray () False (0, n) (zip (filter (<= n) xs) (repeat True))
  where
    n = length xs
-}
