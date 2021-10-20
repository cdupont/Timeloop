{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE PatternSynonyms           #-}

module Tree where

import Data.Map as M hiding (map)
import Data.Maybe
import Data.List
import Data.List.Extra

data Dir = N | S | E | W 
   deriving (Eq, Ord, Show)

data Pos = Pos {
  x :: Int,
  y :: Int,
  t :: Int}
  deriving (Eq, Ord, Show)

type MaxPos = Pos

-- A room
data Room = Room {
  _doors :: Map Dir (Pos, Dir)} -- 4 doors (N, S, E, W) linking to another position with entry direction
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
genRoom p mp = Room $ fromList $ [(d, (move p d, d)) | d <- [N, S, E, W], move p d `isIn` mp]

-- Generate a universe within the limits
genUniv :: MaxPos -> Map Pos Room
genUniv mp@(Pos mx my mt) = fromList $ [(Pos x y t, genRoom (Pos x y t) mp) | t <- [0..mt-1], y <- [0..my-1], x <- [0..mx-1]]

-- A 3*3 universe with a portal
smallU :: Map Pos Room
smallU = M.insert (Pos 2 0 2) (Room (fromList [(E, (Pos 0 0 0, E))])) $ genUniv (Pos 3 3 3)

-- 3*3 universe with angled portal
smallU' :: Map Pos Room
smallU' = M.insert (Pos 2 0 2) (Room (fromList [(E, (Pos 1 2 0, S))])) $ genUniv (Pos 3 3 3)

-- 3*3 universe with angled portal and colision
smallU'' :: Map Pos Room
smallU'' = M.insert (Pos 1 0 2) (Room (fromList [(E, (Pos 1 2 0, S))])) $ genUniv (Pos 3 3 3)

-- straight traversal, no colisions
-- start at a position with a certain direction
trav :: Pos -> Dir -> Map Pos Room -> [(Pos, Dir)]
trav p d mr = (p, d) : rest where
  rest = case M.lookup d doors of 
    Just (p', d') -> trav p' d' mr
    Nothing -> []
  -- doors of the room at position p
  doors :: Map Dir (Pos, Dir)
  doors = _doors (mr ! p) 

-- straight traversal, no colisions
-- start at a position with a certain direction
--trav' :: Pos -> Room -> Map Pos Room -> [(Pos, Room)]
--trav' p r mr = (p, r) : case M.lookup d (_doors (mr ! p)) of  -- Lookup through the doors if one is iin our direction
--                    Just (p', d') -> trav p' (Room (fromList [(p', ())])) mr -- One exists, it leads to p', with direction d'
--                    Nothing -> []
              
validTrav :: [(Pos, (Dir, Dir))] -> Bool
validTrav psdd = and $ Prelude.map (\(a, b) -> validTrav' b) $ groupSort psdd 

-- Transits in a single room
validTrav' :: [(Dir, Dir)] -> Bool
validTrav' []       = True -- Nobody
validTrav' [(i, o)] = i == o -- Simple traversal
validTrav' [a, b]   = b == col a -- collisions

-- collide two travellers
col :: (Dir, Dir) -> (Dir, Dir)
col (N, N) = (N, N) -- Straight for both
col (N, S) = (S, N) 
col (N, E) = (W, N) 
col (N, W) = (E, S) 
col (S, N) = (S, N)
col (S, S) = (S, S) 
col (S, E) = (W, N) 
col (S, W) = (E, N) 
col (E, N) = (S, W)
col (E, S) = (N, W) 
col (E, E) = (E, E) 
col (E, W) = (W, E) 
col (W, N) = (S, E)
col (W, S) = (N, E) 
col (W, E) = (E, W) 
col (W, W) = (W, W) 
