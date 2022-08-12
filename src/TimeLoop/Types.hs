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

-- An Pos is the composition of a place, a time and a direction.
data Pos = Pos {
  x :: Int,
  y :: Int,
  t :: Time,
  d :: Dir}
  deriving (Eq, Ord, Show, Read)

type Path = [Pos]

data Portal = Portal Pos Pos
  deriving (Eq, Ord, Show)

type Univ = [Portal]

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
portal1 = [Portal (Pos 1 (-1) 2 S) (Pos 2 0 0 W)]

--Entrance portal in front, exit up
portal2 :: Univ
portal2 = [Portal (Pos 2 0 2 E) (Pos 1 1 0 S)]

initPos :: Pos
initPos = Pos 0 0 0 E

testTree :: Tree
testTree = Straight $ Bump Front (Straight $ Straight Stop) Stop


