import Control.Lens

data Tree a = Node a (Tree a) (Tree a) | Leaf a
  deriving (Show)

intTree :: Tree Int
intTree = Node 2 (Leaf 3) (Node 5 (Leaf 7) (Leaf 11))
listTree = Node [1,1] (Leaf [2,1]) (Node [3,2] (Leaf [5,2]) (Leaf [7,4])) 
tupleTree = Node (1,1) (Leaf (2,1)) (Node (3,2) (Leaf (5,2)) (Leaf (7,4)))

getRoot :: Tree a -> a
getRoot (Leaf z) = z
getRoot (Node z _ _) = z

setRoot :: Tree a -> a -> Tree a
setRoot (Leaf z) x = Leaf x 
setRoot (Node z lTree rTree) x = Node x lTree rTree

fmapRoot :: (a -> a) -> Tree a -> Tree a
fmapRoot f tree = let newRoot = f (getRoot tree) in setRoot tree newRoot

fmapRoot' f (Leaf z) = Leaf $ f z
fmapRoot' f (Node z lTree rTree) = Node (f z) lTree rTree
setRoot' tree x = fmapRoot' (\_ -> x) tree

main = do
  print $ setRoot intTree 11
  print $ fmapRoot (map (*2)) listTree

  fmapRootIO displayM intTree
  root displayM intTree

-- type Lens' s a = Functor f => (a -> f a) -> s -> f s


-- Writing a Lens
-- lens' :: Functor f => (a -> f a) -> s -> f s

fmapRootIO :: (a -> IO a) -> Tree a -> IO (Tree a)
fmapRootIO g (Leaf z) = (g z) >>= return . Leaf
fmapRootIO g (Node z lTree rTree) = (g z) >>= return . (\x -> Node x lTree rTree)

displayM x = print x >> return x

root :: Functor f => (a -> f a) -> Tree a -> f (Tree a)
root g (Leaf z) = fmap Leaf (g z) -- same as, Leaf <$> (g z)
root g (Node z l r) = fmap (\x -> Node x l r) (g z)

main' = do
  -- GET
  print $ view root listTree
  print $ view root intTree
  -- SET
  print $ set root [42] listTree
  print $ set root 42 intTree
  -- FMAP
  print $ over root (+11) intTree
  print $ over root (++[11]) listTree


-- Composable getters and setters
rightMost :: Functor f => (a -> f a) -> Tree a -> f (Tree a)
rightMost g (Leaf z) = Leaf <$> (g z)
rightMost g (Node z l r) = (\r' -> Node z l r') <$> (rightMost g r)

main'' = do
  -- GET
  print $ view rightMost listTree
  print $ view rightMost intTree
  -- SET
  print $ set rightMost [42] listTree
  print $ set rightMost 42 intTree
  -- FMAP
  print $ over rightMost (+11) intTree
  print $ over rightMost (++[11]) listTree

  -- Compose Getters and Setters
  print $ view (rightMost._1) tupleTree
  print $ set (rightMost._1) 0 tupleTree
  print $ over (rightMost._1) (*100) tupleTree

-- Lens Traversal
-- traversal :: Applicative f => (a -> f a) -> Tree a -> f (Tree a)
leaves :: Applicative f => (a -> f a) -> Tree a -> f (Tree a)
leaves g (Leaf z) = Leaf <$> (g z)
leaves g (Node z l r) = Node z <$> leaves g l <*> leaves g r

main''' = do
  -- Compose Traversal + Lens
  print $ over (leaves._1) (*100) tupleTree
  -- Compose Traversal + Traversal
  print $ over (leaves.both) (*100) tupleTree
  print $ over (leaves.mapped) (*(-1)) listTree
  -- Traversal with effects
  mapMOf leaves displayM tupleTree