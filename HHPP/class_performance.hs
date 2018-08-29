
class Some a where
  next :: a -> a -> a

instance Some Double where
  next a b = (a + b) / 2

-- This version uses slightly less memory and time.
goGeneral :: Some a => Int -> a -> a
goGeneral 0 x = x
goGeneral n x = goGeneral (n - 1) (next x x)

goSpecialized :: Int -> Double -> Double
goSpecialized 0 x = x
goSpecialized n x = goSpecialized (n - 1) (next' x x)

next' :: Double -> Double -> Double
next' a b = (a + b) / 2
