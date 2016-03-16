shiftedDiff :: String -> String -> Int
shiftedDiff a b = n
  where
    k = length a
    rots = map (\i -> (i, rotate i a)) [0..k]
    rots' = filter (\(i, str) -> str == b) $ rots
    n = case rots' of
      [] -> -1
      _ -> minimum $ map fst rots'

rotate :: Int -> [a] -> [a]
rotate n = foldr (.) id (replicate n rotateOne)
  where
    rotateOne [] = []
    rotateOne xs = reverse . (\(a : as) -> as ++ [a]) . reverse $ xs
