{-# LANGUAGE MonadComprehensions #-}

module TimeLoop.Types where

data Dir = N | S | E | W 
   deriving (Eq, Ord, Read)

instance Show Dir where
  show N = "↑ "
  show W = "← "
  show E = "→ "
  show S = "↓ "

type Time = Int

data Pos = Pos {
  x :: Int,
  y :: Int}
  deriving (Eq, Ord, Show, Read)

data PTD = PTD {
  pos  :: Pos,
  time :: Time,
  dir  :: Dir}
  deriving (Eq, Ord, Show, Read)

-- An Walker is a particle with a position, a time and a direction.
type Walker = PTD

-- Complete path followed by a walker
type Path = [PTD]

-- A path segment is a section of path for a given particle
type PathSegment = Path


-- A portal links two points in space, at specific times and directions.
data Portal = Portal PTD PTD 
  deriving (Eq, Ord, Show)

--a Univers is considered inifinite, flat (e.g. normal move laws appy), with some portals.
type Univ = [Portal]

-- The search tree for the trajectory of a particle. The particle can either:
-- - go straight (that includes going through a portal)
-- - bump into itself
data Tree = Straight Tree
          | Bump RelDir Tree Tree
          | Loop Int
          | Stop
          deriving (Eq, Ord, Show)

data RelDir = Front | Back | Right_ | Left_
  deriving (Eq, Ord, Show)

type Limits = ((Int, Int), (Int, Int))


--  sample data *

--Entrance portal below, exit in front
portal1 :: Univ
portal1 = [Portal (PTD (Pos 1 (-1)) 2 S) (PTD (Pos 2 0) 0 W)]

--Entrance portal in front, exit up
portal2 :: Univ
portal2 = [Portal (PTD (Pos 2 0) 2 E) (PTD (Pos 1 1) 0 S)]

initPos :: PTD
initPos = PTD (Pos 0 0) 0 E

testTree :: Tree
testTree = Straight $ Bump Front (Straight $ Straight Stop) Stop


