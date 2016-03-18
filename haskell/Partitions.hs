-- write a function that takes an Integer and returns the number of
-- integer partitions (discounting order)

-- 1 -> 1
-- 2 -> 1+1, 2
-- 3 -> 1+1+1, 1+2, 3
-- 4 -> 1+1+1+1, 1+1+2, 1+3, 2+2, 4
-- look at the pattern for 4
-- you take anything from `parts 3` and you add 1.
-- you take anything from `parts 2` and you add 2.
-- the problem is collision, we need to take out the things in both
-- how can we find the things in both?

parts :: Integer -> Integer
parts 1 = 1
parts n = sum [parts k | k <- [1..(n-1)]] + 1


main :: IO ()
main = test parts partsTests


test :: Show a => Eq b => Show b => (a -> b) -> [(a, b)] -> IO ()
test f tests = mapM_ print . map process $ tests
  where
    process (x, y) = let res = f x in (((x, y), res), res == y)


partsTests :: [(Integer, Integer)]
partsTests =
  [ (1, 1)
  , (2, 2)
  , (3, 3)
  , (4, 5)
  , (5, 7)
  , (6, 11)
  , (7, 15)
  , (8, 22)
  , (9, 30)
  , (10, 42)
  , (11, 56)
  , (12, 77)
  , (13, 101)
  , (14, 135)
  , (15, 176)
  , (16, 231)
  , (17, 297)
  , (18, 385)
  , (19, 490)
  , (20, 627)
  , (21, 792)
  , (22, 1002)
  , (23, 1255)
  , (24, 1575)
  ]
