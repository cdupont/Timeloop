{-# LANGUAGE MonadComprehensions #-}

module Tree2 where

import Prelude hiding (Left, Right)
import Data.List
import Data.Matrix hiding ((<|>))
import qualified Data.Vector             as V
import Data.Maybe (listToMaybe, catMaybes)
import Control.Monad ((>>), guard, join)
import Control.Monad.Omega
import Control.Applicative

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
getPaths (Straight t) p u       =   updateHead (p :)           (getPaths t  (move u p) u)
getPaths (Bump Front t1 t2) p u = ( updateHead (p :)           (getPaths t1 (move u (turn Right_ p)) u)) ++
                                  ( updateHead (turn Back p :) (getPaths t2 (move u (turn Right_ $ turn Back p)) u))
getPaths (Loop i) p u = [[p]] 
getPaths Stop p u = [[p]]

getPaths' :: Pos -> Univ -> Tree -> [[Pos]]
getPaths' p u t = getPaths t p u

allTrees :: [Tree]
allTrees = runOmega allTrees'

allTrees' :: Omega Tree
allTrees' = pure Stop 
       <|> Straight <$> allTrees'
       <|> Bump Front <$> allTrees' <*> allTrees'


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

onePortal :: Univ
onePortal = [Portal (Pos 1 0 2 S) (Pos 2 1 0 W)]

initPos :: Pos
initPos = Pos 0 1 0 E

testTree :: Tree
testTree = Straight $ Bump Front (Straight $ Straight Stop) Stop

-- * Pretty prints *

getPathMatrix :: [Pos] -> Matrix [(Time, Dir)]
getPathMatrix ps = matrix 3 3 f where
  f (x, y) = map (\(Pos _ _ t d) -> (t, d)) $ filter (\(Pos x' y' _ _) -> x-1 == x' && y-1 == y') ps

prettyPath :: Matrix [(Time, Dir)] -> String
prettyPath m = unlines [unwords (fmap (\j -> fill $ strings ! (j, (nrows m) +1 - i)) [1..ncols m]) ++ "\n" | i <- [1..nrows m] ]
 where
   strings = fmap showPos m
   widest = V.maximum $ fmap length (getMatrixAsVector strings)
   fill str = replicate ((widest +1 - length str) `div` 2) ' ' ++ str ++ replicate ((widest +1 - length str) `div` 2) ' '
   blank = fill ""
   showPos ps = intercalate " " $ map (\(t, d) -> show t ++ show d) ps
          
getUnivMatrix :: [Portal] -> Matrix (Maybe (Time, Dir, Bool))
getUnivMatrix ps = matrix 3 3 f where
  f (x, y) = case find (\(Portal (Pos x1 y1 _ _) (Pos x2 y2 _ _)) -> (x-1 == x1 && y-1 == y1) || (x-1 == x2 && y-1 == y2)) ps of
              Just (Portal (Pos x1 y1 t1 d1) (Pos _ _ t2 d2)) | (x-1 == x1 && y-1 == y1) -> Just (t1, d1, True)
              Just (Portal (Pos x1 y1 t1 d1) (Pos _ _ t2 d2)) | (x-1 == x1 && y-1 == y1) -> Just (t2, d2, False)
              Nothing -> Nothing

prettyUniv :: Matrix (Maybe (Int, Dir, Bool)) -> String
prettyUniv m = unlines [unwords (fmap (\j -> fill $ strings ! (j, (nrows m) +1 - i)) [1..ncols m]) ++ "\n" | i <- [1..nrows m] ]
 where
   strings = fmap showPos m
   widest = V.maximum $ fmap length (getMatrixAsVector strings)
   fill str = replicate (widest - length str) ' ' ++ str
   blank = fill ""
   showPos (Just (t, d, s)) = show t ++ show d ++ if s then "□" else "▣"
   showPos Nothing = "."
