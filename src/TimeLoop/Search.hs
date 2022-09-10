{-# LANGUAGE MonadComprehensions #-}

module TimeLoop.Search where

import Prelude hiding (Left, Right)
import Data.List
import Data.Ord
import Data.Maybe (listToMaybe, catMaybes)
import Control.Applicative
import Control.Monad
import TimeLoop.Types
import TimeLoop.Walker
import Control.Monad.WeightedSearch as W


-- Get all blocks that contains valid trajectories for the given universe
getValidSTBlocks :: Univ -> [STBlock]
getValidSTBlocks u = filter isValidBlock $ getAllSTBlocks u

getAllSTBlocks :: Univ -> [STBlock]
getAllSTBlocks u@(Univ _ es cs) = map (\ps -> STBlock u (join $ getAllWalkers $ getTimeline ps es cs)) (getPortalCombinations u)


-- A portal can be used, or not.
-- We simulate all possible combinations of portal usage.
getPortalCombinations :: Univ -> [[Portal]]
getPortalCombinations (Univ ps _ _) = subsequences ps

-- Get a list of exits and entrys, indexed by their time.
getTimeline :: [Portal] -> [Exit] -> [Entry] -> [([Exit], [Entry])]
getTimeline ps emitters consumers = map getIOT [0..maxStep] where
  getIOT t = ((emitters  ++ (map exit ps))  `at_t` t, 
              (consumers ++ (map entry ps)) `at_t` t) where
  at_t ptds t = filter (\(PTD _ t' _) -> t == t') ptds

-- generate the full universe of walkers from a timeline.
getAllWalkers :: [([Exit], [Entry])] -> [[Walker]]
getAllWalkers timeline = zipWith (++) (map fst timeline) (scanl getNextStep [] timeline)

-- Move the walkers one step. Some new walkers can appear (from exits).
-- some walkers can disappear (in entries).
getNextStep :: [Walker] -> ([Exit], [Entry]) -> [Walker]
getNextStep ws (exits, entries) = (concatMap move $ posGroups ((ws ++ exits) \\ entries)) where
  posGroups as = groupBy (\(PTD p1 t1 _) (PTD p2 t2 _) -> p1 == p2 && t1 == t2) as

-- A Universe is valid when a walker that enters a portal, also exits it. 
isValidBlock :: STBlock -> Bool
isValidBlock (STBlock (Univ ps _ _) ws) = and $ map (isValidPortal ws) ps where
  isValidPortal ws (Portal c e) = (c `elem` ws) == (e `elem` ws)



showWalkers :: [Walker] -> String
showWalkers ws = intercalate ";  " (map show ws)


