-- Finished


solution :: String -> Int
solution string = helper 0 string
  where
    helper acc "" = acc
    helper acc (x : "") = acc + charMap x
    helper acc (x : y : rest) =
      case (x, y) of
        ('I', 'V') -> helper (acc + 4) rest
        ('I', 'X') -> helper (acc + 9) rest
        ('X', 'L') -> helper (acc + 40) rest
        ('X', 'C') -> helper (acc + 90) rest
        ('C', 'D') -> helper (acc + 400) rest
        ('C', 'M') -> helper (acc + 900) rest
        _ -> helper (acc + charMap x) (y : rest)


charMap :: Char -> Int
charMap char = case char of
  'I' -> 1
  'V' -> 5
  'X' -> 10
  'L' -> 50
  'C' -> 100
  'D' -> 500
  'M' -> 1000


main :: IO ()
main = print (test solution solutionTests)


test :: Eq b => (a -> b) -> [(a, b)] -> [Bool]
test f = map (\(x, y) -> f x == y)


solutionTests :: [(String, Int)]
solutionTests =
  [ ("XXI", 21)
  , ("I", 1)
  , ("IV", 4)
  ]
