{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Diag where

import Diagrams.Prelude hiding (rotate)
import Diagrams.Backend.SVG.CmdLine
import Data.Graph
import Types
import Data.Graph.Inductive.Graph hiding ((&))
import Data.Graph.Inductive.PatriciaTree
import Control.Monad
import Graph
import Data.Maybe
import Linear.Quaternion
import Text.Printf
import Data.Colour.Palette.BrewerSet

clrs :: [Colour Double]
clrs = brewerSet Reds 3 

showGraph :: Gr Pos Dir -> Diagram B
showGraph g = spots (labNodes g) <> arrows (labEdges g) (labNodes g)

spots :: [LNode Pos] -> Diagram B
spots ls = mconcat $ map (\(n, p) -> spot p <> tex p n) ls where
  spot :: Pos -> Diagram B
  spot p@(Pos _ _ t) = circle 0.02 # lw none # fc (clrs !! t) # moveTo (p2 $ proj p) 
  tex p@(Pos x y t) n = text ((show n) ++ " (" ++ (show x) ++ "," ++ (show y) ++ ")") # fontSizeL 0.1 moveTo (p2 $ proj p) # fc (clrs !! t)

arrows :: [LEdge Dir] -> [LNode Pos] -> Diagram B
arrows es ns = mconcat $ map (\(a, b, l) -> mkArrow (fromJust $ lookup a ns) (fromJust $ lookup b ns) ) es where
  mkArrow :: Pos -> Pos -> Diagram B
  mkArrow pa pb = arrowBetween' (with & headLength .~ normal) (p2 $ proj pa) (p2 $ proj pb) # lc (clrs !! (t pa)) 

shaft  = arc xDir (-1/6 @@ turn)

proj :: Pos -> (Double, Double)
proj (Pos x y t) = (x', y') where
  (V3 x' y' _) = rotate q (V3 (fromIntegral x) (fromIntegral y) (fromIntegral t))
  q = axisAngle (V3 1.0 1.0 0.0) 0.2 


ex :: IO ()
--ex = mainWith $ bg white $ showGraph (fst $ genUniverse 2 2 2) # centerXY # pad 1.1
ex = mainWith $ bg white $ showGraph smallU # centerXY # pad 1.1
