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
import Data.Maybe

showGraph :: Gr Pos Dir -> Diagram B
showGraph g = spots (labNodes g) <> arrows (labEdges g) (labNodes g)

spots :: [LNode Pos] -> Diagram B
spots ls = mconcat $ map (\(n, Pos x y _) -> spot x y <> text ((show n) ++ " (" ++ (show x) ++ "," ++ (show y) ++ ")") # fontSizeL 0.1 moveTo (p2 (fromIntegral x, fromIntegral y))) ls where
  spot :: Int -> Int -> Diagram B
  spot x y = circle 0.02 # lw none # fc blue # moveTo (p2 (fromIntegral x, fromIntegral y))

arrows :: [LEdge Dir] -> [LNode Pos] -> Diagram B
arrows es ns = mconcat $ map (\(a, b, _) -> mkArrow (fromJust $ lookup a ns) (fromJust $ lookup b ns) ) es where
  mkArrow :: Pos -> Pos -> Diagram B
  mkArrow (Pos x1 y1 _) (Pos x2 y2 _) = arrowBetween' (with & headLength .~ verySmall) (p2 (fromIntegral x1, fromIntegral y1)) (p2 (fromIntegral x2, fromIntegral y2))

ex :: IO ()
--ex = mainWith $ bg white $ showGraph (fst $ genUniverse 2 2 2) # centerXY # pad 1.1
ex = mainWith $ bg white $ showGraph smallU # centerXY # pad 1.1
