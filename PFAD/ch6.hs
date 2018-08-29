type Expression = [Term]
type Term = [Factor]
type Factor = [Int]

valExpr :: Expression -> Int
valExpr = sum . map valTerm
valTerm:: Term -> Int
valTerm = product . map valFact
valFact :: Factor -> Int
valFact = foldl1 (\n d -> 10 * n + d)

good :: Int -> Bool
good v = (v == 100)

expressions :: [Int] -> [Expression]
expressions = concatMap partitions . partitions

partitions :: [a] -> [[[a]]]
partitions [] = [[]]
partitions (x:xs) = [[x]:p | p <- ps] ++ [(x:ys):yys | ys:yys <- ps]
  where ps = partitions xs

komachi :: [Int] -> [Expresssion]
komachi = filter (good . valExpr) . expressions
