import Data.Monoid
import qualified Data.Foldable as F
import qualified Control.Monad as M

data Tree a = Node a (Tree a) (Tree a) | Leaf a
  deriving (Show)
foldT :: Monoid a => Tree a -> a
foldT (Leaf x) = x
foldT (Node x lTree rTree) = (foldT lTree) `mappend` x `mappend` (foldT rTree)

aTree = Node (Sum 2) (Leaf (Sum 3)) (Leaf (Sum 5))

foldT' :: Monoid a => (t -> a) -> Tree t -> a
foldT' toMonoid (Leaf x) = toMonoid x
foldT' toMonoid (Node x lTree rTree) = (foldT' toMonoid lTree) 
  `mappend` (toMonoid x) `mappend` (foldT' toMonoid rTree)

aTree' = Node 2 (Leaf 3) (Leaf 5)

main = do
  print $ foldT' Sum aTree'
  print $ foldT' Product aTree'
  print $ foldT' (Any . (==5)) aTree'

instance F.Foldable Tree where
  foldMap toMonoid (Leaf x) = toMonoid x
  foldMap toMonoid (Node x lTree rTree) = (foldMap toMonoid lTree) 
    `mappend` (toMonoid x) `mappend` (foldMap toMonoid rTree)