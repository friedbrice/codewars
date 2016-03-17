{-# LANGUAGE BangPatterns #-}


import Data.Char (toUpper)
import Data.List (foldl')


toCamelCase2 :: String -> String
toCamelCase2 input = helper "" input
  where
    helper !acc str =
      case str of
        [] -> reverse acc
        '-' : [] -> reverse acc
        '_' : [] -> reverse acc
        '-' : c : rest -> helper ((toUpper c) : acc) rest
        '_' : c : rest -> helper ((toUpper c) : acc) rest
        c : rest -> helper (c : acc) rest


toCamelCase :: String -> String
toCamelCase
  = concat
  . toTail (toHead toUpper)
  . cut (\c -> c == '_' || c == '-')


-- | cut a list at each term satisfying a given predicate
--   (deletes the elements where it breaks the list)
cut :: (a -> Bool) -> [a] -> [[a]]
cut p xs =
  case break p xs of
    (ys, []) ->
      ys : []
    (ys, z : zs) ->
      ys : cut p zs


-- | applies a function to the head of a list
toHead :: (a -> a) -> [a] -> [a]
toHead _ [] = []
toHead f (x : xs) = f x : xs


-- | applies a function to the tail of a list
toTail :: (a -> a) -> [a] -> [a]
toTail _ [] = []
toTail f (x : xs) = x : map f xs


main :: IO ()
main = do
  print (test toCamelCase toCamelCaseTests)
  print (test toCamelCase2 toCamelCaseTests)


test :: Eq b => (a -> b) -> [(a, b)] -> [Bool]
test f = map (\(x, y) -> f x == y)


toCamelCaseTests :: [(String, String)]
toCamelCaseTests =
  [ ("the_stealth_warrior", "theStealthWarrior")
  , ("The-Stealth-Warrior", "TheStealthWarrior")
  , ("all-along-the-watchtower", "allAlongTheWatchtower")
  , ("And_then-came_a-curious_existence", "AndThenCameACuriousExistence")
  ]
