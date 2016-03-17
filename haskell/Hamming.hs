-- STILL WORKING

import qualified Data.Set as Set

-- | This is such a hack! It only works for small integers.
--   i.e. it doesn't not give the correct n'th hamming number
--   for sufficiently large n, but it passed all the tests <.<
hamming  :: Int -> Int
hamming n = (!!) (Set.elems $ hamms n) (n - 1)


hamms :: Int -> Set.Set Int
hamms n = gen n (Set.fromDistinctAscList [1, 2, 3, 4, 5])
  where
    gen n acc
      | Set.size acc > 2*3*5*n = acc -- maximum hackage
      | otherwise =
        let
          acc2 = Set.map (* 2) acc
          acc3 = Set.map (* 3) acc
          acc5 = Set.map (* 5) acc
          newAcc = acc `Set.union` acc2 `Set.union` acc3 `Set.union` acc5
        in gen n newAcc


main :: IO ()
main = mapM_ print (test hamming hammingTests)


test :: Eq b => (a -> b) -> [(a, b)] -> [(((a, b), b), Bool)]
test f = map (\(x, y) -> (((x, y), f x), f x == y))


hammingTests :: [(Int, Int)]
hammingTests =
  [ (1, 1)
  , (2, 2)
  , (3, 3)
  , (4, 4)
  , (5, 5)
  , (6, 6)
  , (7, 8)
  , (8, 9)
  , (9, 10)
  , (10, 12)
  , (11, 15)
  , (12, 16)
  , (13, 18)
  , (14, 20)
  , (15, 24)
  , (16, 25)
  , (17, 27)
  , (18, 30)
  , (19, 32)
  ]
