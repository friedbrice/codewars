-- Still Working

-- works, but hits the memory wall when i submit.
-- going to read up on optimization, then come back.


fix :: (a -> a) -> a
fix f = let x = f x in x


reverse' :: ([a] -> [a]) -> [a] -> [a]
reverse' f list = case list of
  [] -> []
  _ -> (last list) : (f $ init list)


foldr'
  :: ((a -> b -> b) -> b -> [a] -> b)
  -> (a -> b -> b) -> b -> [a] -> b
foldr' g f acc list = case list of
  [] -> acc
  (x : xs) -> g f (f x acc) xs
