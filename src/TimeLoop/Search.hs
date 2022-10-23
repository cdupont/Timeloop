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
import Data.Array
import Control.Scanl
import Control.Monad.State

-- Get all blocks that contains valid trajectories for the given universe
getValidSTBlocks :: Univ -> [STBlock]
getValidSTBlocks u = filter isValidBlock $ getAllSTBlocks u

getAllSTBlocks :: Univ -> [STBlock]
getAllSTBlocks u@(Univ ps es cs) = map getSTBlock $ getPortalCombinations ps where
   getSTBlock :: [Source] -> STBlock
   getSTBlock scs = STBlock u $ join $ elems $ getAllWalkers $ getTimeline (scs ++ es) (cs ++ map entry ps)


-- A portal can emit a walker, or not.
-- We simulate all possible combinations of portal usage.
getPortalCombinations :: [Portal] -> [[Source]]
getPortalCombinations ps = subsequences $ map exit ps 

-- Get a list of Sources and Sinks, indexed by their time.
getTimeline :: [Source] -> [Sink] -> Array Time ([Source], [Sink])
getTimeline emitters consumers = listArray (0, maxStep) $ map getIOT [0..maxStep] where
  getIOT t = (filter (\(Source (PTD _ t' _)) -> t == t') emitters, 
              filter (\(Sink (PTD _ t' _)) -> t == t') consumers) 

-- generate the full universe of walkers from a timeline.
getAllWalkers :: Array Time ([Source], [Sink]) -> Array Time [Walker]
getAllWalkers timeline = snd $ mapAccumL getNextStep [] timeline

-- Move the walkers one step. Some new walkers can appear (from exits).
-- some walkers can disappear (in entries).
getNextStep :: [Walker] -> ([Source], [Sink]) -> ([Walker], [Walker])
getNextStep ws (sources, sinks) = (concatMap move $ posGroups $ (ws ++ emitted) \\ consummed,   -- Will be used by mapAccumL as input for the next step 
                                   ws ++ emitted)                                               -- Will be stored by mapAccumL in the final array
  where
  posGroups as = groupBy (\(Walker (PTD p1 t1 _)) (Walker (PTD p2 t2 _)) -> p1 == p2 && t1 == t2) as
  consummed = map (Walker . unSink) sinks
  emitted = map (Walker . unSource) sources

-- A Universe is valid when a walker that enters a portal, also exits it. 
isValidBlock :: STBlock -> Bool
isValidBlock (STBlock (Univ ps _ _) ws) = all (isValidPortal ws) ps where
  isValidPortal ws (Portal (Sink sk) (Source sc)) = (sc `elem` ws') == (sk `elem` ws')
  ws' = map unWalker ws

dupe :: a -> (a,a)
dupe x = (x,x)

