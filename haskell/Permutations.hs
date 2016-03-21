-- still working

module Codewars.Kata.Permutations (permutations) where

-- In this kata you have to create all permutations of an input string
-- and remove duplicates, if present. This means, you have to shuffle
-- all letters from the input in all possible orders.

-- Examples:

-- permutations    "a" `shouldBe` ["a"]
-- permutations   "ab" `shouldBe` ["ab", "ba"]
-- permutations "aabb" `shouldBe`
--   ["aabb","abab","abba","baab","baba","bbaa"]
-- The order of the permutations doesn't matter.

import Data.List (nub)


permutations :: String -> [String]
permutations = perms


swap :: [a] -> (i, j) -> [a]
swap (i, j) xs = [xs !! (idx k) | k <- [0..(length xs - 1)]]
  where
    idx l
      | l == i = j
      | l == j = i
      | otherwise = l


perms :: Eq a => [a] -> [[a]]
perms xs = nub . error "TODO"
  where
    l = length xs
    idx = [(i, j) | i <- [0..l], k <- [0..l], i < j


-- "abc" >> swap 0 0 == "abc"
-- "abc" >> swap 0 1 == "bac" >> swap 0 2 == "cab"
--                            >> swap 1 2 == "bca"
-- "abc" >> swap 0 2 == "cba" >> swap 0 1 == "bca"
--                            >> swap 1 2 == "cab"
-- "abc" >> swap 1 2 == "acb" >> swap 0 1 == "cab"
--                            >> swap 0 2 == "bca"
