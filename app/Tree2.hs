{-# LANGUAGE MonadComprehensions #-}

module Tree2 where

import Prelude hiding (Left, Right)
import Data.List
import Data.Ord
import Data.Matrix hiding ((<|>))
import qualified Data.Vector             as V
import Data.Maybe (listToMaybe, catMaybes)
import Control.Monad ((>>), guard, join)
import Control.Monad.Omega
import Control.Applicative
import Data.List.Split

data Dir = N | S | E | W 
   deriving (Eq, Ord)

instance Show Dir where
  show N = "↑"
  show W = "←"
  show E = "→"
  show S = "↓"

type Time = Int
type SpaceCoord = Int 

data Pos = Pos {
  x :: SpaceCoord,
  y :: SpaceCoord,
  t :: Time,
  d :: Dir}
  deriving (Eq, Ord, Show)

type Path = [Pos]

data Portal = Portal Pos Pos
  deriving (Eq, Ord, Show)

type Univ = [Portal]

data Tree = Straight Tree
          | Bump RelDir Tree Tree
          | Loop Int
          | Stop
          deriving (Eq, Ord, Show)


move :: Univ -> Pos -> Pos
move u p = case find (\(Portal a _) -> a == p) u of 
                   Just (Portal _ p) -> p
                   Nothing -> simpleMove p

simpleMove :: Pos -> Pos
simpleMove (Pos x y t N) = Pos x (y+1) (t+1) N
simpleMove (Pos x y t S) = Pos x (y-1) (t+1) S
simpleMove (Pos x y t E) = Pos (x+1) y (t+1) E
simpleMove (Pos x y t W) = Pos (x-1) y (t+1) W

data RelDir = Front | Back | Right_ | Left_
  deriving (Eq, Ord, Show)

turn :: RelDir -> Pos -> Pos
turn rd (Pos x y t d) = Pos x y t (turn' rd d)

turn' :: RelDir -> Dir -> Dir
turn' Right_ N = E 
turn' Right_ E = S 
turn' Right_ S = W 
turn' Right_ W = N
turn' Back a   = turn' Right_ $ turn' Right_ a
turn' Left_ a  = turn' Right_ $ turn' Back a
turn' Front a  = a

getPaths :: Tree -> Pos -> Univ -> [[Pos]]
getPaths (Straight t) p u      =  updateHead (p :) (getPaths t  (move u p) u)
getPaths (Bump from t1 t2) p u = (updateHead (p :) (getPaths t1 (move u (turn Right_ p)) u)) ++
                                 (updateHead (p':) (getPaths t2 (move u (turn Right_ p')) u)) where
                                 p' = turn Back $ turn from p
getPaths (Loop i) p u = [[p]] 
getPaths Stop p u = [[p]]

getPaths' :: Pos -> Univ -> Tree -> [[Pos]]
getPaths' p u t = getPaths t p u

allTrees :: [Tree]
allTrees = runOmega allTrees'

allTrees' :: Omega Tree
allTrees' = pure Stop 
       <|> Straight    <$> allTrees'
       <|> Bump Front  <$> allTrees' <*> allTrees'
       <|> Bump Right_ <$> allTrees' <*> allTrees'
       <|> Bump Left_  <$> allTrees' <*> allTrees'


allPaths :: Pos -> Univ -> [Path]
allPaths p u = join $ map chains $ map (getPaths' p u) allTrees

chains :: [Path] -> [Path]
chains ps = catMaybes $ map joinPaths $ permutations ps 

joinPaths :: [Path] -> Maybe Path
joinPaths ps = loop ps 
  where
    loop []       = Nothing 
    loop [a]      = Just a
    loop (x:y:zs) = if (last x == head y) 
                    then loop ((x ++ (tail y)) : zs)
                    else Nothing



updateHead _ []       = []
updateHead f (a : as) = f a : as



--  sample data *

--Entrance portal below, exit in front
portal1 :: Univ
portal1 = [Portal (Pos 1 (-1) 2 S) (Pos 2 0 0 W)]

--Entrance portal in front, exit up
portal2 :: Univ
portal2 = [Portal (Pos 2 0 2 E) (Pos 1 1 0 S)]

initPos :: Pos
initPos = Pos 0 0 0 E


testTree :: Tree
testTree = Straight $ Bump Front (Straight $ Straight Stop) Stop

-- * Pretty prints *


prettyUniv :: Univ -> String
prettyUniv ps = pretty "." $ concatMap showPortal ps where
  showPortal (Portal (Pos x1 y1 t1 d1) (Pos x2 y2 t2 d2)) = [(x1, y1, show t1 ++ show d1 ++ "□"), (x2, y2, show t2 ++ show d2 ++ "▣" )]

prettyPath :: [Pos] -> String
prettyPath ps = pretty "." $ map (\(Pos x y t d) -> (x, y, show t ++ show d)) ps

pretty :: String -> [(Int, Int, String)] -> String
pretty def ps = unlines $ reverse $ map unwords $ chunksOf (maxX - minX +1) $ padStrings strings
 where
  (maxX, _, _) = maximumBy (\(x1, _, _) (x2, _, _) -> compare x1 x2) ps
  (minX, _, _) = minimumBy (\(x1, _, _) (x2, _, _) -> compare x1 x2) ps
  (_, maxY, _) = maximumBy (\(_, y1, _) (_, y2, _) -> compare y1 y2) ps
  (_ ,minY, _) = minimumBy (\(_, y1, _) (_, y2, _) -> compare y1 y2) ps
  strings :: [String]
  strings = [getString ps def (x, y) | x <- [minX..maxX], y <- [minY..maxY]]


getString :: [(Int, Int, String)] -> String -> (Int, Int) -> String
getString ps def (x, y) = case filter (\(x', y', _) -> x == x' && y == y') ps of
  [] -> def
  as -> concatMap (\(_, _, s) -> s) as

padStrings :: [String] -> [String]
padStrings ss = map fill ss where
  widest = maximum $ (map length) ss
  fill str = let pad = (widest +1 - length str) `div` 2 
             in replicate pad ' ' ++ str ++ replicate pad ' '
  

