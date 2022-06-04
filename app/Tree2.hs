
module Tree2 where

import Prelude hiding (Left, Right)
import Data.List
import Data.Matrix
import qualified Data.Vector             as V


data Dir = N | S | E | W 
   deriving (Eq, Ord, Show)

data Pos = Pos {
  x :: Int,
  y :: Int,
  t :: Int}
  deriving (Eq, Ord, Show)


data Portal = Portal (Pos, Dir) (Pos, Dir)

type Univ = [Portal]

data Tree = Straight Tree
          | Bump RelDir Tree Tree Tree
          | Loop Int
          | Stop
          deriving (Eq, Ord, Show)


move :: Univ -> (Pos, Dir) -> (Pos, Dir)
move u (p, d) = case find (\(Portal a _) -> a == (p, d)) u of 
                   Just (Portal _ (p, d)) -> (p, d)
                   Nothing -> simpleMove (p, d)

simpleMove :: (Pos, Dir) -> (Pos, Dir)
simpleMove (Pos x y t, N) = (Pos x (y+1) (t+1), N)
simpleMove (Pos x y t, S) = (Pos x (y-1) (t+1), S)
simpleMove (Pos x y t, E) = (Pos (x+1) y (t+1), E)
simpleMove (Pos x y t, W) = (Pos (x-1) y (t+1), W)

data RelDir = Front | Back | Right_ | Left_
  deriving (Eq, Ord, Show)

turn :: RelDir -> Dir -> Dir
turn Right_ N = E 
turn Right_ E = S 
turn Right_ S = W 
turn Right_ W = N
turn Back a = turn Right_ $ turn Right_ a
turn Left_ a = turn Right_ $ turn Back a
turn Front a = a

turn' :: RelDir -> (Pos, Dir) -> (Pos, Dir)
turn' rd (p, d) = (p, turn rd d)

getTrav :: Tree -> (Pos, Dir) -> Univ -> [Pos]
getTrav (Straight t) (p, d) u = p : (getTrav t (move u (p,d)) u)
getTrav (Bump Front t1 t2 t3) (p, d) u = [p] ++ (          getTrav t3 (move u (p, turn Right_ d)) u)
                                             ++ (reverse $ getTrav t1 (move u (p, d) ) u)
                                             ++ (          getTrav t2 (move u (p, turn Left_ d) ) u)
                                          
getTrav (Loop i) (p, d) u = [p] 
getTrav Stop (p, d) u = [p]

onePortal :: Univ
onePortal = [Portal (Pos 1 0 2, S) (Pos 2 1 0, W)]

initPos :: (Pos, Dir)
initPos = (Pos 0 1 0, E)

testTree :: Tree
testTree = Straight $ Bump Front Stop Stop Stop

getMatrix :: [Pos] -> Matrix (Maybe Int)
getMatrix ps = matrix 3 3 f where
  f (x, y) = case find (\(Pos x' y' _) -> x-1 == x' && y-1 == y') ps of
              Just (Pos _ _ t) -> Just t 
              Nothing -> Nothing

prettyPath :: Matrix (Maybe Int) -> String
prettyPath m = concat
   [
   unlines [unwords (fmap (\j -> fill $ strings ! (j, (nrows m) +1 - i)) [1..ncols m]) | i <- [1..nrows m] ]
   ]
 where
   strings = fmap showPos m
   widest = V.maximum $ fmap length (getMatrixAsVector strings)
   fill str = replicate (widest - length str) ' ' ++ str
   blank = fill ""
   showPos (Just t) = show t
   showPos Nothing = ""
          
