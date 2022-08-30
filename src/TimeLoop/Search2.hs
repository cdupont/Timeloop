{-# LANGUAGE MonadComprehensions #-}

module TimeLoop.Search2 where

import Prelude hiding (Left, Right)
import Data.List
import Data.Ord
import Data.Maybe (listToMaybe, catMaybes)
import Control.Monad ((>>), guard, join)
import Control.Applicative
import Control.Monad
import TimeLoop.Pretty
import Control.Monad.WeightedSearch as W

data Dir = N | S | E | W 
   deriving (Eq, Ord, Read)

instance Show Dir where
  show N = "↑ "
  show W = "← "
  show E = "→ "
  show S = "↓ "

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
  deriving (Eq, Ord, Show)

-- A Univers contains some portals linking distant points in the spacetime block.
-- It also contains emitters and consumers which are point emitting or consuming one walker.
data Univ = Univ {
  portals :: [Portal],
  emitters :: [Exit],
  consumers :: [Entry]}
  deriving (Eq, Ord, Show)

-- A STBlock is infinite and flat spacetime block universe.
-- It contains some "Walkers" which are particules that moves in a straight line.
data STBlock = STBlock {
  univ :: Univ,
  walkers :: [Walker]}
  deriving (Eq, Show)


-- A portal can be used, or not.
-- We simulate all possible combinations of portal usage.
getPortalCombinations :: Univ -> [[Portal]]
getPortalCombinations (Univ ps _ _) = subsequences ps

-- Get a list of exits and entrys, indexed by their time.
getTimeline :: [Portal] -> [Exit] -> [Entry] -> [([Exit], [Entry])]
getTimeline ps emitters consumers = map getIOT [0..10] where
  getIOT t = ((emitters  ++ (map exit ps))  `at_t` t, 
              (consumers ++ (map entry ps)) `at_t` t) where
  at_t ptds t = filter (\(PTD _ t' _) -> t == t') ptds

-- generate the full universe of walkers from a timeline.
getAllWalkers :: [([Exit], [Entry])] -> [[Walker]]
getAllWalkers timeline = scanl getNextStep [] timeline

-- Move the walkers one step. Some new walkers can appear (from exits).
-- some walkers can disappear (in entries).
getNextStep :: [Walker] -> ([Exit], [Entry]) -> [Walker]
getNextStep ws (exits, entries) = (concatMap move $ posGroups ((ws ++ exits) \\ entries)) where
  posGroups as = groupBy (\(PTD p1 _ _) (PTD p2 _ _) -> p1 == p2) as

getAllSTBlocks :: Univ -> [STBlock]
getAllSTBlocks u@(Univ _ es cs) = map (\ps -> STBlock u (join $ getAllWalkers $ getTimeline ps es cs)) (getPortalCombinations u)


-- A Universe is valid when a walker that enters a portal, also exits it. 
isValidUniv :: [Walker] -> [Portal] -> Bool
isValidUniv = undefined

-- A Portal is valid if, when a walker enters a portal, it also exits it. 
isValidPortal :: [Walker] -> Portal -> Bool
isValidPortal = undefined

--Move walkers **at the same position**
move :: [Walker] -> [Walker]
move (w:[]) = [simpleMove w]
move (w1:w2:[]) = [simpleMove $ turn Right_ w1, 
                   simpleMove $ turn Right_ w2]

-- Move one step in a flat universe.
simpleMove :: Walker -> Walker
simpleMove (PTD (Pos x y) t N) = PTD (Pos x (y+1)) (t+1) N
simpleMove (PTD (Pos x y) t S) = PTD (Pos x (y-1)) (t+1) S
simpleMove (PTD (Pos x y) t E) = PTD (Pos (x+1) y) (t+1) E
simpleMove (PTD (Pos x y) t W) = PTD (Pos (x-1) y) (t+1) W

-- Turn a walker using a relative direction
turn :: RelDir -> Walker -> Walker
turn rd (PTD p t d) = PTD p t (turn' rd d)

-- Turn an absolute direction using a relative one
turn' :: RelDir -> Dir -> Dir
turn' Right_ N = E 
turn' Right_ E = S 
turn' Right_ S = W 
turn' Right_ W = N
turn' Back a   = turn' Right_ $ turn' Right_ a
turn' Left_ a  = turn' Right_ $ turn' Right_ $ turn' Right_ a
turn' Front a  = a
--getFirsts :: Univ2 -> [[Walker]]
--getFirsts (Univ2 ps es _) = map (es ++) (subsequences $ map exit ps) 
--
--nextStep :: [PTD] -> Time -> [Walker] -> [Walker]
--nextStep exits t ws = ws ++ (map simpleMove ws') where
--  ws' = filter (\(PTD _ t' _) -> t == t') (ws \\ exits)
--
--getFinalWalkers :: [PTD] -> [Walker] -> [Walker]
--getFinalWalkers exits ws = foldl (\a f -> f a) ws nextSteps where
--  nextSteps = map (nextStep exits) [0..10]
--
--getAllSTBlocks :: Univ2 -> [STBlock]
--getAllSTBlocks u@(Univ2 ps _ cs) = map (\ws -> STBlock u ws) finals where
--  finals = map (getFinalWalkers entries) (getFirsts u)
--  entries = (map entry ps) ++ cs


--Entrance portal below, exit in front
univ1 :: Univ
univ1 = Univ
  [Portal (PTD (Pos 1 (-1)) 2 S) (PTD (Pos 2 0) 0 W)]
  [PTD (Pos 0 0) 0 E]
  []

showWalkers :: [Walker] -> String
showWalkers ws = intercalate ";  " (map show ws)


