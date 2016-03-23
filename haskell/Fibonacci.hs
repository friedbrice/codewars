fib :: Integer -> Integer
fib n
  | n >= 0 = fibs !! (fromIntegral n)
  | otherwise = bifs !! (fromIntegral $ abs n)


fibs :: [Integer]
fibs = [0, 1] ++ [fibs !! (n - 1) + fibs !! (n - 2) | n <- [2..]]


bifs :: [Integer]
bifs = [0, 1] ++ [bifs !! (n - 2) - bifs !! (n - 1) | n <- [2..]]
