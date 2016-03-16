-- FINISHED

isMonotone :: Ord a => [a] -> Bool
isMonotone xs = and . comparisons $ xs

comparisons :: Ord a => [a] -> [Bool]
comparisons xs =
  case xs of
    [] -> []
    x : [] -> []
    x : y : zs -> (x < y) : comparisons zs
