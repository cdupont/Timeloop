{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE PatternSynonyms           #-}

module Tree where

import Data.Map as M hiding (map, take, filter)
import Data.Maybe
import Data.List
import Data.List.Extra hiding (iterate')
import Data.Tuple
import qualified Control.Monad.HT as HT

data Dir = N | S | E | W 
   deriving (Eq, Ord, Show)

data Pos = Pos {
  x :: Int,
  y :: Int,
  t :: Int}
  deriving (Eq, Ord, Show)

type MaxPos = Pos

newtype InDir = InDir Dir
  deriving (Eq, Ord, Show)

newtype OutDir = OutDir Dir
  deriving (Eq, Ord, Show)

-- A room
data Room = Room {
  _doors :: Map OutDir (Pos, InDir)} -- 4 doors (N, S, E, W) linking to another position with entry direction
  deriving (Eq, Ord, Show)

-- Universe is a collection of rooms indexed by their position
type Univ = Map Pos Room

-- Check if a position is in the universe
isIn :: Pos -> MaxPos -> Bool
isIn (Pos x y t) (Pos mx my mt) = x >= 0 && x < mx &&
                                  y >= 0 && y < my &&
                                  t >= 0 && t < mt
move :: Pos -> Dir -> Pos
move (Pos x y t) N = (Pos x (y+1) (t+1))
move (Pos x y t) S = (Pos x (y-1) (t+1))
move (Pos x y t) E = (Pos (x+1) y (t+1))
move (Pos x y t) W = (Pos (x-1) y (t+1))

-- Generate a room with links to other rooms
genRoom :: Pos -> MaxPos -> Room
genRoom p mp = Room $ fromList $ [(OutDir d, (move p d, InDir d)) | d <- [N, S, E, W], move p d `isIn` mp]

-- Generate a universe within the limits
genUniv :: MaxPos -> Map Pos Room
genUniv mp@(Pos mx my mt) = fromList $ [(Pos x y t, genRoom (Pos x y t) mp) | t <- [0..mt-1], y <- [0..my-1], x <- [0..mx-1]]

-- A 3*3 universe with no portal
smallU :: Map Pos Room
smallU = genUniv (Pos 3 3 3)

-- 3*3 universe with angled portal
smallU' :: Map Pos Room
smallU' = M.insert (Pos 2 0 2) (Room (fromList [(OutDir E, (Pos 1 2 0, InDir S))])) $ genUniv (Pos 3 3 3)

-- 3*3 universe with colision
smallU'' :: Map Pos Room
smallU'' = M.insert (Pos 1 0 2) (Room (fromList [(OutDir S, (Pos 1 2 0, InDir S))])) $ genUniv (Pos 3 3 3)

-- straight traversal, no colisions
-- start at a position with a certain direction
--trav :: Move -> Map Pos Room -> [Move]
--trav start univ = start : unfoldr findNext start where
--   findNext (p, (d1, d2)) = case M.lookup d2 $ _doors (univ ! p) of
--               Just next -> Just (next, next)
--               Nothing -> Nothing

-- Transit through a room
type Transit = (Pos, (InDir, OutDir))
type Path = [Transit]
        
-- valid traversal of several rooms         
validTrav :: [Transit] -> Bool
validTrav ts = and $ map (\(_, t) -> validTransRoom t) $ groupSort ts

-- valid Transits through a single room (there can be several transits colliding)
validTransRoom :: [(InDir, OutDir)] -> Bool
validTransRoom []       = True -- Nobody
validTransRoom [(InDir i, OutDir o)] = i == o -- Simple straight traversal
validTransRoom [(InDir i1, OutDir o1), (InDir i2, OutDir o2)] = o1 == i2 && o2 == i1  -- collisions
validTransRoom _ = False 


allTravs :: Transit -> Map Pos Room -> [Path]
allTravs cur univ = bfPaths nextTrans cur where 
  nextTrans :: Transit -> [Transit]
  nextTrans (p1, (i1, o1)) = map (\o -> (p2, (i2, o))) $ keys $ _doors $ univ ! p2 where
    (p2, i2) = (_doors $ univ ! p1) ! o1

dfPaths :: (a -> [a]) -> a -> [[a]]
dfPaths f a = map (a:) ([] : concatMap (dfPaths f) (f a))

bfPaths :: (a -> [a]) -> a -> [[a]]
bfPaths f a = go [(a, [a])] where
  go []              =  []
  go ((s, path) : q) = path : go (q ++ [ (x, path ++ [x]) | x <- f s ])

pathToU :: [(Pos, (InDir, OutDir))] -> Map Pos Room -> Map Pos Room
pathToU ts univ = fromListWith merge $ map (\(p, (i, o)) -> (p, Room (fromList [(o, (_doors $ univ ! p) ! o)]))) ts where
  merge :: Room -> Room -> Room
  merge (Room ds1) (Room ds2) = Room (M.union ds1 ds2)

--data Room = Room {
--   _doors :: Map OutDir (Pos, InDir)} 

showTransit :: Transit -> String
showTransit ((Pos x y t), (InDir i, OutDir o)) = (show i) ++ "->(" ++ (show x) ++ "," ++ (show y) ++ "," ++ (show t) ++ ")->" ++ (show o)

showPath :: Path -> String
showPath ts = concat $ intersperse " ~ " (map showTransit ts)


exampleTravs = allTravs (Pos 0 1 0, (InDir E, OutDir E)) smallU''
goodTravs = filter validTrav (take 1000 exampleTravs)
showGoodTravs = putStrLn $ concat $ intersperse "\n" (map showPath goodTravs)



