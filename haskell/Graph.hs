-- FINISHED
--module Graph where

import Control.Applicative ((<$>), (<*>))
import qualified Data.Set as S


type Node = Char
type Arc  = (Node, Node)


solveGraph :: Node -> Node -> [Arc] -> Bool
solveGraph s e arcs = s == e || S.member (s, e) graph'
  where
    graph = S.fromList arcs
    graph' = iterate compose graph !! (length arcs + 1)
    compose g = S.union g g'
      where
        arcs = S.toList g
        arcs' = filter (\(x, y) -> x == s) arcs
        g'
          = S.map (\((a, b), (c, d)) -> (a, d))
          $ S.filter (\((a, b), (c, d)) -> b == c)
          $ S.fromList
          $ (,) <$> arcs' <*> arcs


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
  --where
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
