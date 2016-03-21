module CanYouGetTheLoop where
--import CanYouGetTheLoop.Types

-- an algorithm for finding a cycle in a linked list.
-- runs in O(n^2) time, I think. Your list starts at
-- some node, and you run shortCircuit on the list
-- `iterate next`
shortCircuit :: Eq a => [a] -> Int
shortCircuit = helper 0 []
  where
    helper n ys (x : xs) =
      if not $ x `elem` ys
      then helper (n + 1) (x : ys) xs
      else n - length (takeWhile (/= x) $ reverse ys)
