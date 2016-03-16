-- FINISHED

-- | We use Rational because Double is prone to rounding errors.
type Point = (Rational, Rational)


-- | Decide whether all points lie on a line.
onLine :: [Point] -> Bool
onLine ps =
  case ps of
    [] -> True
    p : [] -> True
    p1 : p2 : [] -> True
    p1 : p2 : p3 : rest ->
      colinear p1 p2 p3 && onLine (p2 : p3 : rest)


colinear :: Point -> Point -> Point -> Bool
colinear p1@(x1, y1) p2@(x2, y2) p3@(x3, y3) =
  let
    [u1, u2] = [x2 - x1, y2 - y1]
    [v1, v2] = [x3 - x1, y3 - y1]
  in
    if ([u1, u2] == [0, 0]) || ([v1, v2] == [0, 0])
    then True
    else case [u1 /= 0, u2 /= 0, v1 /= 0, v2 /= 0] of
      [True, _, _, _] ->
        let a = v1 / u1
        in a * u2 == v2
      [_, True, _, _] ->
        let a = v2 / u2
        in a * u1 == v1
      [_, _, True, _] ->
        let a = u1 / v1
        in a * v2 == u2
      [_, _, _, True] ->
        let a = u2 / v2
        in a * v1 == u1


test :: Eq b => (a -> b) -> [(a, b)] -> [Bool]
test f tests = [f x == y | (x, y) <- tests]


main :: IO ()
main = do
  print (test (uncurry . uncurry $ colinear) colinearTests)
  print (test onLine onLineTests)


colinearTests :: [(((Point, Point), Point), Bool)]
colinearTests =
  [ ((((1, 2), (7, 4)), (22, 9)), True)
  , ((((1, 2), (-3, -14)), (22, 9)), False)
  , ((((1, 1), (0, 0)), (1, -1)), False)
  ]


onLineTests :: [([Point], Bool)]
onLineTests =
  [ ([(3, 0), (3, 2)], True)
  , ([(1, 2), (7, 4), (22, 9)], True)
  , ([(1, 2), (-3, -14), (22, 9)], False)
  , ([(1, 1), (0, 0), (1, -1)], False)
  , ([(1, 4), (1, 4), (3, 4), (17, 4), (34, 4)], True)
  , ([(1, 4), (3, 4), (17, 4), (34, 4)], True)
  ]
