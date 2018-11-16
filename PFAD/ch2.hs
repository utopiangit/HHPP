-- Chapter 2 maximum surpasser count
import Data.List

msc :: Ord a => [a] -> Int
msc xs = maximum [scount z zs | z:zs <- tails xs]
scount x xs = length $ filter (x<) xs

msc' :: Ord a => [a] -> Int
msc' = maximum . map snd . table

table :: (Ord a) => [a] -> [(a, Int)]
table [x] = [(x, 0)]
table xs = join (m - n) (table ys) (table zs)
  where
    m = length xs
    n = m `div` 2
    (ys, zs) = splitAt n xs

join 0 txs [] = txs
join n [] tys = tys
join n txs@((x, c):txs') tys@((y, d):tys')
  | x < y = (x, c + n):join n txs' tys
  | otherwise = (y, d):join (n - 1) txs tys'
