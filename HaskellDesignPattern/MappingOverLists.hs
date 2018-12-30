import Data.Monoid
import qualified Data.Foldable as F
import qualified Control.Monad as M

doF n = do
  print n
  return (n * 2)

main = do
  let l = [2,3,5,7]
  print $ map (*2) l
  mapM doF l >>= print
  mapM_ doF l

mapA :: Applicative f => (a -> f t) -> [a] -> f [t]
mapA f = sequenceA' . (map f)

sequenceA' :: Applicative f => [f t] -> f [t]
sequenceA' [] = pure []
sequenceA' (x:xs) = (:) <$> x <*> (sequenceA' xs)

mapA' f [] = pure []
mapA' f (x:xs) = (:) <$> f x <*> (mapA' f xs)

main' = mapA' doF [2,3,5,7] >>= print
