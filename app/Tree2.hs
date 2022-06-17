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
import Control.Monad

data Dir = N | S | E | W 
   deriving (Eq, Ord)

instance Show Dir where
  show N = "↑ "
  show W = "← "
  show E = "→ "
  show S = "↓ "

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

-- sample searchs

search1 :: [Path]
search1 = filter (\l -> length l == 6) $ allPaths initPos portal1

pretty1 :: String
pretty1 = concatMap (prettyUnivPath lims portal1) search1

pretty1' :: [String]
pretty1' = map (\t -> prettyUnivPath lims portal1 $ filterTime (head search1) t) [0..] 

filterTime :: Path -> Time -> Path
filterTime ps t = filter (\(Pos _ _ t' _) -> t == t') ps

stepper :: IO ()
stepper = forM_ pretty1' (\s -> do
  getChar
  putStr "\ESC[2J"
  putStrLn s)
  

-- * Pretty prints *

type Limits = ((Int, Int), (Int, Int))

lims :: Limits
lims = ((-3, -3), (3, 3))

prettyUniv' :: Univ -> String
prettyUniv' u = prettyUniv u (getLimits $ concatMap (\(Portal p1 p2) -> [p1, p2]) u)

prettyUniv :: Univ -> Limits -> String
prettyUniv ps l = pretty "." l $ showUniv ps

showUniv :: Univ -> [(Int, Int, String)] 
showUniv ps = concatMap (\(Portal (Pos x1 y1 t1 d1) (Pos x2 y2 t2 d2)) -> [(x1, y1, show t1 ++ show d1 ++ "□ "), (x2, y2, show t2 ++ show d2 ++ "▣ " )]) ps 


prettyPath :: Limits -> [Pos] -> String
prettyPath l ps = pretty "." l $ showPos ps 

showPos :: [Pos] -> [(Int, Int, String)] 
showPos ps = map (\(Pos x y t d) -> (x, y, show t ++ show d)) ps


prettyUnivPath :: Limits -> Univ -> [Pos] -> String
prettyUnivPath l u ps = pretty "." l $ showPos ps ++ showUniv u

-- Pretty prints a list of coordinates as a matrix.
pretty :: String -> Limits -> [(Int, Int, String)] -> String
pretty def ((minX, minY), (maxX, maxY)) ps = unlines $ reverse $ map unwords $ chunksOf (maxX - minX +1) $ padStrings strings
 where
  strings :: [String]
  strings = [getString ps def (x, y) | y <- [minY..maxY], x <- [minX..maxX]]


getString :: [(Int, Int, String)] -> String -> (Int, Int) -> String
getString ps def (x, y) = case filter (\(x', y', _) -> x == x' && y == y') ps of
  [] -> def
  as -> concatMap (\(_, _, s) -> s) as

padStrings :: [String] -> [String]
padStrings ss = map fill ss where
  --widest = maximum $ (map length) ss
  widest = 9
  fill str = replicate ((widest +1 - length str) `div` 2) ' ' ++ str ++ replicate ((widest  - length str) `div` 2) ' '
  
getLimits :: [Pos] -> Limits 
getLimits ps = ((minX, minY), (maxX, maxY)) where
  (Pos maxX _ _ _) = maximumBy (\(Pos x1 _ _ _) (Pos x2 _ _ _) -> compare x1 x2) ps
  (Pos minX _ _ _) = minimumBy (\(Pos x1 _ _ _) (Pos x2 _ _ _) -> compare x1 x2) ps
  (Pos _ maxY _ _) = maximumBy (\(Pos _ y1 _ _) (Pos _ y2 _ _) -> compare y1 y2) ps
  (Pos _ minY _ _) = minimumBy (\(Pos _ y1 _ _) (Pos _ y2 _ _) -> compare y1 y2) ps


