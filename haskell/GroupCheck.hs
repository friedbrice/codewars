-- FINISHED


groupCheck :: String -> Bool
groupCheck input =
  let subs = iterate removeSubformula input in
  stableTail subs == ""


removeSubformula :: String -> String
removeSubformula = helper ""
  where
    helper acc str =
      case str of
        "" -> acc
        '(' : ')' : rest -> acc ++ rest
        '[' : ']' : rest -> acc ++ rest
        '{' : '}' : rest -> acc ++ rest
        x : rest -> helper (acc ++ [x]) rest


-- | Unsafe.
stableTail :: Eq a => [a] -> a
stableTail (x : y : ys) =
  if x == y
  then x
  else stableTail (y : ys)


main :: IO ()
main = print (test groupCheck groupCheckTests)


test :: Eq b => (a -> b) -> [(a, b)] -> [Bool]
test f = map (\(x, y) -> f x == y)


groupCheckTests :: [(String, Bool)]
groupCheckTests =
  [ ("{()[]}", True)
  , ("{([)]}", False)
  , ("[()[]]", True)
  ]
