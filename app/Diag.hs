{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Diag where

import Diagrams.Prelude hiding (rotate, E, start)
import Diagrams.Backend.SVG.CmdLine
import Control.Monad
import Tree
import Data.Maybe
import Linear.Quaternion
import Text.Printf
import Data.Colour.Palette.BrewerSet
import Data.Map hiding (map)
import Diagrams.TwoD.Arrow

-- Color set
clrs :: [Colour Double]
clrs = brewerSet Reds 3 

--Show the complete graph
showGraph :: Map Pos Room -> Diagram B
showGraph m = spots (keys m) <> arrows (toList m)

--draw the spots and text
spots :: [Pos] -> Diagram B
spots ps = mconcat $ map (\p -> spot p <> tex p) ps where
  spot :: Pos -> Diagram B
  spot p@(Pos _ _ t) = circle 0.02 # lw none # fc (clrs !! t) # moveTo (proj $ toP3 p) 
  tex p@(Pos x y t)  = text (" (" ++ (show x) ++ "," ++ (show y) ++ "," ++ (show t) ++ ")") # fontSizeL 0.1 moveTo (proj $ toP3 p) # fc (clrs !! t)

--draw the arrows
arrows :: [(Pos, Room)] -> Diagram B
arrows rs = mconcat $ map (\(p1, (Room ds))-> doorArrows p1 (toList ds)) rs where

--draw the arrows for a door
doorArrows :: Pos -> [(OutDir, (Pos, InDir))] -> Diagram B
doorArrows p1@(Pos _ _ t) ds = mconcat $ map (\(d1, (p2, d2)) -> mkArrow (proj $ toP3 p1, d1) (proj $ toP3 p2, d2) t) ds


-- Projection and rotation
toP3 :: Pos -> P3 Double
toP3 (Pos x y t) = p3 (fromIntegral x, fromIntegral y, fromIntegral t)

proj :: P3 Double -> P2 Double 
proj p = origin .+^ v ^._xy where
  v = rotate q (p .-. origin)
  q = axisAngle (V3 1.0 1.0 0.0) 1 

-- control points for bézier curves
control :: Dir -> V2 Double
control N = r2 (0, 0.5)
control S = r2 (0, -0.5)
control E = r2 (0.5, 0)
control W = r2 (-0.5, 0)

-- shaft of arrows
shaft :: (P2 Double, OutDir) -> (P2 Double, InDir) ->  Located (Trail V2 Double)
shaft (p, OutDir d) (p', InDir d') = trailFromSegments [bézier3 (control d) ((p' .-. p) - (control d')) (p' .-. p)] `at` p

-- create a single arrow
mkArrow :: (P2 Double, OutDir) -> (P2 Double, InDir) -> Int -> Diagram B
mkArrow a b col = arrowFromLocatedTrail' (with & arrowHead .~ dart
                              & lengths .~ veryLarge
                              & shaftStyle %~ lw thick) (shaft a b) # lc (clrs !! col)

col1 :: Diagram B
col1 = showGraph (pathToU (goodTravs start smallUCol !!2) smallUCol ) 

ex :: IO ()
--ex = mainWith $ bg white $ showGraph (genUniv (Pos 2 2 2)) # centerXY # pad 1.1
--ex = mainWith $ bg white $ showGraph smallU' # centerXY # pad 1.1
ex = mainWith $ bg white $ col1 # centerXY # pad 1.1
