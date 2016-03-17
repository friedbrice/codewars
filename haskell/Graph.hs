--module Graph where


import Data.Set (elems, fromList, member, Set)


type Node = Char
type Arc  = (Node, Node)


solveGraph :: Node -> Node -> [Arc] -> Bool
solveGraph s e arcs =
  case s == e of
    True -> True
    False -> (s, e) `member` graph'
  where
    graph = fromList arcs
    graph' = iterate compose graph !! (length arcs)
    compose g =
      let g' = elems g in
      fromList $ [(s, c) | (s, b) <- g', (b, c) <- g']


main :: IO ()
main = mapM_ print
  (zip [1..] $ test (uncurry . uncurry $ solveGraph) solveGraphTests)


test :: Eq b => (a -> b) -> [(a, b)] -> [Bool]
test f = map (\(x, y) -> f x == y)


solveGraphTests :: [(((Node, Node), [Arc]), Bool)]
solveGraphTests =
  [ ((('a', 'a'), g1), True)
  , ((('a', 'b'), g1), True)
  , ((('a', 'c'), g1), False)
  , ((('a', 'd'), g2), True)
  , ((('a', 'e'), g2), False)
  , ((('a', 'a'), g2), True)
  , ((('a', 'b'), g2), True)
  , ((('a', 'c'), g2), True)
  , ((('2', 'U'), g3), False)
  ]
  where
    g1 = [('a', 'b')]
    g2 =
      [ ('a', 'b')
      , ('b', 'c')
      , ('c', 'a')
      , ('c', 'd')
      , ('e', 'a')
      ]
    g3 =
      [ ('2', 'a')
      , ('b', 'U')
      ]
