{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import Lib
import Data.Graph
import Data.Graph.Inductive.Query.DFS
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.GVD
import Data.Graph.Inductive.NodeMap
import Data.Tree

data Dir = U | D | R | L
   deriving (Eq, Ord, Show)

data Pos = Pos {
  x :: Int,
  y :: Int,
  t :: Int}
  deriving (Eq, Ord)

data Mobile = Mobile Pos Dir

instance Show Pos where
  show (Pos x y t) = show (x, y, t)

test :: Gr Pos Dir
test = mkGraph [(0, Pos 0 0 0), (1, Pos 0 1 0), (2, Pos 1 0 0), (3, Pos 1 1 0),
                (4, Pos 0 0 1), (5, Pos 0 1 1), (6, Pos 1 0 1), (7, Pos 1 1 1)]
               [(0, 6, R), (0, 5, U), (1, 7, R), (1, 4, D),
                (2, 4, L), (2, 7, U), (3, 5, L), (3, 6, D), (6, 3, R)]

smallU :: Gr Pos Dir
smallU = insMapEdge nm portal g where
  (g, nm) = genUniverse 2 2 2
  portal :: (Pos, Pos, Dir)
  portal = (Pos 1 0 2, Pos 1 2 0, D)

genUniverse :: Int -> Int -> Int -> (Gr Pos Dir, NodeMap Pos)
genUniverse x y t = mkMapGraph ps ls where
  ps = genPoses x y t
  ls = filter (\(_, b, _) -> b `elem` ps) $ concatMap links ps

genPoses :: Int -> Int -> Int -> [Pos]
genPoses x_max y_max t_max = [(Pos x y t) | x <- [0..x_max], y <- [0..y_max], t <- [0..t_max]]

move :: Pos -> Dir -> Pos
move (Pos x y t) U = (Pos x (y+1) (t+1))
move (Pos x y t) D = (Pos x (y-1) (t+1))
move (Pos x y t) R = (Pos (x+1) y (t+1))
move (Pos x y t) L = (Pos (x-1) y (t+1))

links :: Pos -> [(Pos, Pos, Dir)]
links p = (p, move p U, U)
        : (p, move p D, D)
        : (p, move p L, L)
        : (p, move p R, R)
        : []


isValid :: Path -> Bool
isValid = undefined


trav :: CFun Pos Dir [Node]
trav (_, _, _, outs) = map snd $ filter (\(a, _) -> a == R) outs

res :: CFun Pos Dir Pos
res (_, _, a, _) = a

f a = xdfsWith trav res [a] test 

-- Valid transits
validTrans :: Context Pos Dir -> Bool
-- No transit at all
validTrans ([], _, _, []) = True
-- Going in, going out
validTrans ([(R, _)], _, _, [(R, _)]) = True
validTrans ([(L, _)], _, _, [(L, _)]) = True
validTrans ([(U, _)], _, _, [(U, _)]) = True
validTrans ([(D, _)], _, _, [(D, _)]) = True
-- Collisions
validTrans ([(R, _), (D, _)], _, _, [(D, _), (R, _)]) = True
validTrans _ = False


main :: IO ()
main = do
  let a = Pos 0 0 0
  let b = Pos 1 0 0
  let (graph, nodeFromVertex, vertexFromKey) = graphFromEdges [(a, a, [b]), (a, a, [b]), (b, b, [])]
  putStrLn $ show graph
  print $ vertices graph
