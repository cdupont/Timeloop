{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Diag where

import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine
import Data.Graph
import Types
import Data.Graph.Inductive.Graph hiding ((&))
import Data.Graph.Inductive.PatriciaTree
import Control.Monad
import Graph

sPt = p2 (10.20, 10.20)
ePt = p2 (2.85, 0.85)

-- We use small blue and red circles to mark the start and end points.
spot :: Diagram B
spot = circle 0.02 # lw none
sDot = spot # fc blue # moveTo sPt
eDot = spot # fc red  # moveTo ePt

example :: Diagram B 
example = ( sDot <> eDot <> arrowBetween' (with & headLength .~ veryLarge) sPt ePt) # centerXY # pad 1.1

showDiag :: Gr Pos Dir -> Diagram B
showDiag g = grid (labNodes g) <> arrows (labEdges g)

grid :: [LNode Pos] -> Diagram B
--grid ls = mconcat $ map (\(_, Pos x y _) -> sDot) ls
grid ls = mconcat $ map (\(_, Pos x y _) -> spot # fc blue # moveTo (p2 (fromIntegral x, fromIntegral y))) ls

mkArrow :: Pos -> Pos -> Diagram B
mkArrow (Pos x1 y1 _) (Pos x2 y2 _) = arrowBetween' (with & headLength .~ veryLarge) (p2 (fromIntegral x1, fromIntegral y1)) (p2 (fromIntegral x2, fromIntegral y2))

arrows :: [LEdge Dir] -> Diagram B
arrows es = mconcat $ map (\(a, b, _) -> mkArrow (toPos 2 2 2 a) (toPos 2 2 2 b) ) es

ex :: IO ()
--ex = mainWith $ sDot # centerXY # pad 1.1
--ex = mainWith $ grid [(0, Pos 1 1 1)] # centerXY # pad 1.1
--ex = mainWith $ bg white $ showDiag (fst $ genUniverse 2 2 2) # centerXY # pad 1.1
ex = mainWith $ bg white $ showDiag (test) # centerXY # pad 1.1
