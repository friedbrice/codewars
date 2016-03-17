-- FINISHED

import Data.List

findMissing (x : y : z : rest) =
  if y - x == z - y
  then findMissing (y : z : rest)
  else missing
    where
      delta = (min (abs $ y - x) (abs $ z - y)) * signum (y - x)
      sqnce = [x + delta * n | n <- [1,2,3]]
      missing = head $ sqnce \\ [y, z]
