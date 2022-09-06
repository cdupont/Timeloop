{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}

module TimeLoop.Types where

import GHC.Generics (Generic)
import Optics.Label

data Dir = N | S | E | W 
   deriving (Eq, Ord, Show)

data RelDir = Front | Back | Right_ | Left_
  deriving (Eq, Ord, Show)

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


