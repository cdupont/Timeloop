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
  deriving (Eq, Ord, Show, Generic)

data PTD = PTD {
  pos  :: Pos,
  time :: Time,
  dir  :: Dir}
  deriving (Eq, Ord, Show, Generic)

--instance Show PTD where
--  show (PTD (Pos x y) t d) = show x ++ ", " ++ show y ++ ", " ++ show t ++ " " ++ (show d)

-- An Walker is a particle with a position, a time and a direction.
newtype Walker = Walker {unWalker :: PTD}
  deriving (Eq, Ord, Show, Generic)

newtype Source = Source {unSource :: PTD}
  deriving (Eq, Ord, Show, Generic)

newtype Sink = Sink {unSink :: Pos}
  deriving (Eq, Ord, Show, Generic)

-- A portal links two points in space, at specific times and directions.
data Portal = Portal {
  entry :: Sink,
  exit  :: Source}
  deriving (Eq, Ord, Show, Generic)

-- A Univers contains some portals linking distant points in the spacetime block.
-- It also contains emitters and consumers which are point emitting or consuming one walker.
data Univ = Univ {
  portals :: [Portal],
  emitters :: [Source],
  consumers :: [Sink]}
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

initPos :: Dir -> Pos
initPos N = Pos 1 0
initPos S = Pos (-1) 0
initPos E = Pos 0 1
initPos W = Pos 0 (-1)


--  sample data *

initSource :: Source
initSource = Source (PTD (Pos 0 0) 0 E)

source1 :: Source
source1 = Source (PTD (Pos 1 (-1)) 0 N)

source2 :: Source
source2 = Source (PTD (Pos 0 3) 0 E)

walker1 :: Walker 
walker1 = Walker (PTD (Pos 0 0) 0 E)

walker2 :: Walker 
walker2 = Walker (PTD (Pos 0 0) 0 N)

walker3 :: Walker 
walker3 = Walker (PTD (Pos 5 5) 0 N)

portal1 :: Portal
portal1 = Portal (Sink $ Pos 0 0) (Source (PTD (Pos 1 0) 1 W))

--No solution (self deviating)
univ1 :: Univ
univ1 = Univ {portals = [Portal {entry = Sink $ Pos {x = 6, y = 0}, exit = Source {unSource = PTD {pos = Pos {x = 3, y = 3}, time = 0, dir = S}}}], emitters = [Source {unSource = PTD {pos = Pos {x = 0, y = 0}, time = 0, dir = E}}], consumers = []}

--two solutions: going straight or going through portal
univ2 :: Univ
univ2 = Univ
  [Portal (Sink $ Pos 3 (-3)) (Source (PTD (Pos 6 0) 0 W))]
  [Source (PTD (Pos 0 0) 0 E)]
  []

--createRelPortal :: RelDir -> Int -> Portal
--createRelPortal 

-- The Djinn
univ3 :: Univ
univ3 = Univ {portals = [Portal {entry = Sink $ Pos {x = 6, y = 0}, exit = Source {unSource = PTD {pos = Pos {x = 0, y = 0}, time = 0, dir = E}}}], emitters = [], consumers = []}

--One solution: Deviate a Djinn 
univ4 :: Univ
univ4 = Univ {portals = [Portal {entry = Sink $ Pos {x = 6, y = 0}, exit = Source {unSource = PTD {pos = Pos {x = 3, y = -3}, time = 0, dir = N}}}], emitters = [Source {unSource = PTD {pos = Pos {x = 0, y = 0}, time = 0, dir = E}}], consumers = []}

-- The northern cross
univ5 :: Univ
univ5 = Univ {portals = [Portal {entry = Sink $ Pos {x = 3, y = -3}, exit = Source {unSource = PTD {pos = Pos {x = 6, y = 0}, time = 0, dir = W}}}, Portal {entry = Sink $ Pos {x = 3, y = 3}, exit = Source {unSource = PTD {pos = Pos {x = 0, y = 0}, time = 0, dir = E}}}], emitters = [], consumers = []}

-- Kill one solution
univ6 :: Univ
univ6 = Univ {portals = [Portal {entry = Sink $ Pos {x = 3, y = -3}, exit = Source {unSource = PTD {pos = Pos {x = 6, y = 0}, time = 0, dir = W}}}, Portal {entry = Sink $ Pos {x = 3, y = 3}, exit = Source {unSource = PTD {pos = Pos {x = 1, y = 2}, time = 3, dir = E}}}], emitters = [Source {unSource = PTD {pos = Pos {x = 0, y = 0}, time = 0, dir = E}}], consumers = []}

-- 4 solutions
univ7 :: Univ
univ7 = Univ {portals = [Portal {entry = Sink $ Pos {x = 3, y = -3}, exit = Source {unSource = PTD {pos = Pos {x = 5, y = -1}, time = 0, dir = W}}}, Portal {entry = Sink $ Pos {x = 5, y = 1}, exit = Source {unSource = PTD {pos = Pos {x = 1, y = 1}, time = 2, dir = E}}}], emitters = [Source {unSource = PTD {pos = Pos {x = 1, y = -1}, time = 0, dir = E}}], consumers = []}

