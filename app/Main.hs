{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Lib
import Data.Graph
import Data.Graph.Inductive.Query.DFS
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Query.GVD
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

move :: Pos -> Dir -> Pos
move (Pos x y t) U = (Pos x (y+1) (t+1))
move (Pos x y t) D = (Pos x (y-1) (t+1))
move (Pos x y t) R = (Pos (x+1) y (t+1))
move (Pos x y t) L = (Pos (x-1) y (t+1))

links :: Pos -> [(Pos, Pos)]
links p = (p, move p U) 
        : (p, move p D)
        : (p, move p L)
        : (p, move p R)
        : []

type Link = (Pos, Pos)


type Universe = ([Pos], [Link])

genUniverse :: Int -> Int -> Int -> Universe
genUniverse x y t = (ps, prune ps $ concatMap links ps) where
   ps = genPoses x y t
   prune :: [Pos] -> [(Pos, Pos)] -> [(Pos, Pos)]
   prune ps ls = filter (\(a, b) -> elem b ps) ls

genPoses :: Int -> Int -> Int -> [Pos]
genPoses x_max y_max t_max = [(Pos x y t) | x <- [0..x_max], y <- [0..y_max], t <- [0..t_max]]

test :: Gr Pos Dir
test = mkGraph [(0, Pos 0 0 0), (1, Pos 0 1 0), (2, Pos 1 0 0), (3, Pos 1 1 0),
                (4, Pos 0 0 1), (5, Pos 0 1 1), (6, Pos 1 0 1), (7, Pos 1 1 1)]
               [(0, 6, R), (0, 5, U), (1, 7, R), (1, 4, D),
                (2, 4, L), (2, 7, U), (3, 5, L), (3, 6, D), (6, 0, R)]

trav :: CFun Pos Dir [Node]
trav (_, _, _, outs) = map snd $ filter (\(a, _) -> a == R) outs

res :: CFun Pos Dir Pos
res (_, _, a, _) = a

f a = xdfsWith trav res [a] test 

main :: IO ()
main = do
  let a = Pos 0 0 0
  let b = Pos 1 0 0
  let (graph, nodeFromVertex, vertexFromKey) = graphFromEdges [(a, a, [b]), (a, a, [b]), (b, b, [])]
  putStrLn $ show graph
  print $ vertices graph
