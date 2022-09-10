{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}

module TimeLoop.Types where

import GHC.Generics (Generic)
import Optics.Label

data Dir = N | E | S | W 
   deriving (Eq, Ord, Show, Enum, Bounded)

data RelDir = Front | Back | Right_ | Left_
  deriving (Eq, Ord, Show, Enum, Bounded)

type Time = Int

data Pos = Pos {
  x :: Int,
  y :: Int}
  deriving (Eq, Ord, Show)

data PTD = PTD {
  pos  :: Pos,
  time :: Time,
  dir  :: Dir}
  deriving (Eq, Ord)

instance Show PTD where
  show (PTD (Pos x y) t d) = show x ++ ", " ++ show y ++ ", " ++ show t ++ " " ++ (show d)

-- An Walker is a particle with a position, a time and a direction.
type Walker = PTD
type Exit = PTD
type Entry = PTD

-- A portal links two points in space, at specific times and directions.
data Portal = Portal {
  entry :: Entry,
  exit  :: Exit}
  deriving (Eq, Ord, Show, Generic)

-- A Univers contains some portals linking distant points in the spacetime block.
-- It also contains emitters and consumers which are point emitting or consuming one walker.
data Univ = Univ {
  portals :: [Portal],
  emitters :: [Exit],
  consumers :: [Entry]}
  deriving (Eq, Ord, Show, Generic)

-- A STBlock is infinite and flat spacetime block universe.
-- It contains some "Walkers" which are particules that moves in a straight line.
data STBlock = STBlock {
  univ :: Univ,
  walkers :: [Walker]}
  deriving (Eq, Show, Generic)

type Limits = ((Int, Int), (Int, Int))

maxStep :: Int
maxStep = 10

--  sample data *

initPos :: PTD
initPos = PTD (Pos 0 0) 0 E

portal1 :: Portal
portal1 = Portal (PTD (Pos 0 0) 0 S) (PTD (Pos 1 0) 1 W)

--Entrance portal below, exit in front
--two solutions: going straight or goign through portal
univ1 :: Univ
univ1 = Univ
  [Portal (PTD (Pos 3 (-3)) 6 S) (PTD (Pos 6 0) 0 W)]
  [PTD (Pos 0 0) 0 E]
  []

--One solution: going through portal (self-rightning solution)
univ2 :: Univ
univ2 = Univ
  [Portal (PTD (Pos 2 0) 2 E) (PTD (Pos 1 (-1)) 0 N)]
  [PTD (Pos 0 0) 0 E]
  []

--No solution (self deviating)
univ3 :: Univ
univ3 = Univ
  [Portal (PTD (Pos 2 0) 2 E) (PTD (Pos 1 1) 0 S)]
  [PTD (Pos 0 0) 0 E]
  []
