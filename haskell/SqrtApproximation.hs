-- FINISHED

import Prelude hiding (sqrt)

sqrtInt :: Integral n => n -> Either (n,n) n
sqrtInt n =
  if y == n
  then Right x
  else Left (x - 1, x)
  where
    (_, (x, y) : _) = break (\(x, y) -> y >= n) squares
    squares = [(k, k^2) | k <- [1..]]
