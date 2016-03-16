-- STILL WORKING

groupCheck :: String -> Bool
groupCheck [] = True
groupCheck input =
  case break isOpening input of
    (_, []) -> -- no opening delimiters
      case break isClosing input of
        (_, []) -> True -- no delimiters
        _ -> False -- closing but no opening
    (_, x : xs) -> -- x is an opening delimiter
      case break (isCloserOf x) xs of
        (_, []) -> False -- no closer for x
        (ys, z : zs) -> -- z closes x
          groupCheck ys && groupCheck zs


isOpening :: Char -> Bool
isOpening c =
  case c of
    '(' -> True
    '[' -> True
    '{' -> True
    _ -> False


isClosing :: Char -> Bool
isClosing c =
  case c of
    ')' -> True
    ']' -> True
    '}' -> True
    _ -> False


isCloserOf :: Char -> Char -> Bool
isCloserOf c =
  case c of
    '(' -> (== ')')
    '[' -> (== ']')
    '{' -> (== '}')
    _ -> error $ 'c' : " is not a delimiter!"
