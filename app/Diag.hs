{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Diag where

import Diagrams.Prelude hiding (rotate)
import Diagrams.Backend.SVG.CmdLine
import Control.Monad
import Tree
import Data.Maybe
import Linear.Quaternion
import Text.Printf
import Data.Colour.Palette.BrewerSet
import Data.Map hiding (map)

clrs :: [Colour Double]
clrs = brewerSet Reds 3 

showGraph :: Map Pos Room -> Diagram B
showGraph m = spots (keys m) <> arrows (toList m)

spots :: [Pos] -> Diagram B
spots ps = mconcat $ map (\p -> spot p <> tex p) ps where
  spot :: Pos -> Diagram B
  spot p@(Pos _ _ t) = circle 0.02 # lw none # fc (clrs !! t) # moveTo (p2 $ proj p) 
  tex p@(Pos x y t)  = text (" (" ++ (show x) ++ "," ++ (show y) ++ "," ++ (show t) ++ ")") # fontSizeL 0.1 moveTo (p2 $ proj p) # fc (clrs !! t)

arrows :: [(Pos, Room)] -> Diagram B
arrows rs = mconcat $ map (\(p1, (Room ds)) -> doorArrows p1 (toList ds)) rs where

doorArrows :: Pos -> [(Dir, (Pos, Dir))] -> Diagram B
doorArrows p1 ds = mconcat $ map (\(d1, (p2, d2)) -> mkArrow (p1, d1, d2, p2)) ds

mkArrow :: (Pos, Dir, Dir, Pos) -> Diagram B
mkArrow (pa, da, db, pb)  = arrowBetween' (with & headLength .~ normal) (p2 $ proj pa) (p2 $ proj pb) # lc (clrs !! (t pa))
                      -- ||| text (show d1) # moveTo (p2 $ proj p1)


shaft  = arc xDir (-1/6 @@ turn)

toV3 :: Pos -> V3 Double
toV3 (Pos x y t) = undefined --p3 (fromIntegral x, fromIntegral y, fromIntegral t)


proj' :: V3 Double -> V2 Double 
proj' p3 = p3' ^._xy where
  p3' = rotate q p3
  q = axisAngle (V3 1.0 1.0 0.0) 0.2 

proj :: Pos -> (Double, Double) 
proj (Pos x y t) = (x', y') where
  (V3 x' y' _) = rotate q (V3 (fromIntegral x) (fromIntegral y) (fromIntegral t))
  q = axisAngle (V3 1.0 1.0 0.0) 0.2 


ex :: IO ()
--ex = mainWith $ bg white $ showGraph (fst $ genUniverse 2 2 2) # centerXY # pad 1.1
ex = mainWith $ bg white $ showGraph smallU' # centerXY # pad 1.1
