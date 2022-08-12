
module TimeLoop.Pretty where

import Prelude hiding (Left, Right)
import Data.List
import Data.Ord
import Data.Matrix hiding ((<|>))
import qualified Data.Vector             as V
import Data.Maybe (listToMaybe, catMaybes)
import Control.Monad ((>>), guard, join)
import Control.Monad.Omega
import Control.Applicative
import Data.List.Split
import Control.Monad
import TimeLoop.Types

-- * Pretty prints *


lims :: Limits
lims = ((-3, -3), (3, 3))

prettyUniv' :: Univ -> String
prettyUniv' u = prettyUniv u (getLimits $ concatMap (\(Portal p1 p2) -> [p1, p2]) u)

prettyUniv :: Univ -> Limits -> String
prettyUniv ps l = pretty "." l $ showUniv ps

showUniv :: Univ -> [(Int, Int, String)] 
showUniv ps = concatMap (\(Portal (Pos x1 y1 t1 d1) (Pos x2 y2 t2 d2)) -> [(x1, y1, show t1 ++ show d1 ++ "\n□ "), (x2, y2, show t2 ++ show d2 ++ "\n▣ " )]) ps 


prettyPath :: Limits -> [Pos] -> String
prettyPath l ps = pretty "." l $ showPos ps 

showPos :: [Pos] -> [(Int, Int, String)] 
showPos ps = map (\(Pos x y t d) -> (x, y, show t ++ show d)) ps


prettyUnivPath :: Limits -> Univ -> [Pos] -> String
prettyUnivPath l u ps = pretty "." l $ showPos ps ++ showUniv u

-- Pretty prints a list of coordinates as a matrix.
pretty :: String -> Limits -> [(Int, Int, String)] -> String
pretty def ((minX, minY), (maxX, maxY)) ps = unlines $ reverse $ map unwords $ chunksOf (maxX - minX +1) $ padStrings strings
 where
  strings :: [String]
  strings = [getString ps def (x, y) | y <- [minY..maxY], x <- [minX..maxX]]


getString :: [(Int, Int, String)] -> String -> (Int, Int) -> String
getString ps def (x, y) = case filter (\(x', y', _) -> x == x' && y == y') ps of
  [] -> def
  as -> concatMap (\(_, _, s) -> s) as

padStrings :: [String] -> [String]
padStrings ss = map fill ss where
  --widest = maximum $ (map length) ss
  widest = 9
  fill str = replicate ((widest +1 - length str) `div` 2) ' ' ++ str ++ replicate ((widest  - length str) `div` 2) ' '
  
getLimits :: [Pos] -> Limits 
getLimits ps = ((minX, minY), (maxX, maxY)) where
  (Pos maxX _ _ _) = maximumBy (\(Pos x1 _ _ _) (Pos x2 _ _ _) -> compare x1 x2) ps
  (Pos minX _ _ _) = minimumBy (\(Pos x1 _ _ _) (Pos x2 _ _ _) -> compare x1 x2) ps
  (Pos _ maxY _ _) = maximumBy (\(Pos _ y1 _ _) (Pos _ y2 _ _) -> compare y1 y2) ps
  (Pos _ minY _ _) = minimumBy (\(Pos _ y1 _ _) (Pos _ y2 _ _) -> compare y1 y2) ps


