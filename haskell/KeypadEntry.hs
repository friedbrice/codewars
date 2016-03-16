import Data.List (foldl')


presses :: String -> Int
presses = foldl' (\acc c -> acc + (evalMap keymap c)) 0


-- | Unsafe.
evalMap :: Eq a => Eq b => [([a], b)] -> a -> b
evalMap f x = head . map snd . filter (\(ks, v) -> x `elem` ks) $ f


-- | Maps an upper-case letter or a space
--   to the number of keypresses required.
keymap :: [([Char], Int)]
keymap =
  [ ("aAdDgGjJmMpPtTwW 1", 1)
  , ("bBeEhHkKnNqQuUxX0", 2)
  , ("cCfFiIlLoOrRvVyY", 3)
  , ("sSzZ234568", 4)
  , ("79", 5)
  ]
