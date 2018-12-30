

data Tree a = Node a (Tree a) (Tree a) | Leaf a
  deriving (Show) 

instance Functor Tree where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node x lTree rTree) = Node (f x) (fmap f lTree) (fmap f rTree)

instance Foldable Tree where
  foldMap f (Leaf x) = f x
  foldMap f (Node x lTree rTree) = (foldMap f lTree) 
    `mappend` (f x) `mappend` (foldMap f rTree)

instance Traversable Tree where
  traverse g (Leaf x) = Leaf <$> (g x)
  traverse g (Node x lTree rTree) = Node <$> (g x) 
    <*> (traverse g lTree) <*> (traverse g rTree)

aTree = Node 2 (Leaf 3) (Node 5 (Leaf 7) (Leaf 11))

main = traverse doF aTree
  where doF n = do print n; return (n * 2)