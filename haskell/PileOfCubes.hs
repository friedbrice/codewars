-- STIL WORKING

--findNb :: Integer -> Integer
--findNb m =
--  case squareRoot m of
--    Nothing -> -1
--    Just k ->
--      case rectangleRoot $ 2 * k of
--        Nothing -> -1
--        Just n -> n

--squareRoot :: Integer -> Maybe Integer
--squareRoot 0 = Just 0
--squareRoot n =
--  if y == n
--    then Just x
--    else Nothing
--  where
--    (_, (x, y) : _) = break (\(x, y) -> y >= n) squares
--    squares = [(k, k^2) | k <- [1..]]

--rectangleRoot :: Integer -> Maybe Integer
--rectangleRoot 0 = Just 0
--rectangleRoot n =
--  if y == n
--    then Just x
--    else Nothing
--  where
--    (_, (x, y) : _) = break (\(x, y) -> y >= n) rectangles
--    rectangles = [(k, k * (k + 1)) | k <- [1..]]

import Control.DeepSeq

findNb :: Integer -> Integer
findNb m = helper 0
  where
    helper k = k `deepseq`
      let
        s0 = id $! (k * (k + 1)) ^ 2
        s1 = id $! 4 * m
      in
        if (s0 == s1) `deepseq` (s0 == s1)
        then k
        else
          if (s0 > s1) `deepseq` (s0 > s1)
          then -1
          else helper (k + 1)
