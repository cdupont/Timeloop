{-# LANGUAGE MonadComprehensions #-}

module TimeLoop.Search where

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
import TimeLoop.Pretty

-- Search the possible paths for a walker in a given universe, until a max depth.
search :: Walker -> Univ -> Int -> [Path]
search p u depth = filter (\l -> length l == depth) $ getAllPaths p u

-- get all possible paths in a universe for a given walker
getAllPaths :: Walker -> Univ -> [Path]
getAllPaths p u = join $ map getPaths $ map (\t -> getPathSegments t p u) allTrees

-- Generate the path segments for a tree in the given universe, using the start position
getPathSegments :: Tree -> Walker -> Univ -> [PathSegment]
getPathSegments Stop p u = [[p]]
getPathSegments (Straight t) p u      =  updateHead (p :) (getPathSegments t  (move u p) u)
getPathSegments (Bump from t1 t2) p u = (updateHead (p :) (getPathSegments t1 (move u (turn Right_ p)) u)) ++
                                        (updateHead (p':) (getPathSegments t2 (move u (turn Right_ p')) u)) where
                                        p' = turn Back $ turn from p

-- get all possible paths from a list of path segments
getPaths :: [PathSegment] -> [Path]
getPaths ps = catMaybes $ map getPath $ permutations ps 


-- Find out if path segments matches in order to create a long path
-- The end of a segment should match the beginning of the next
getPath :: [PathSegment] -> Maybe Path
getPath ps = foldM f [] ps where
  f [] a = Just a
  f a [] = Just a
  f x y = if (last x == head y)
            then Just $ x ++ (tail y)
            else Nothing

-- Generate all tree from the Omega monad.
allTrees :: [Tree]
allTrees = runOmega allTrees'

-- The Omega monad performs a Breadth first generation of the search tree.
-- It will be used lazily by the function getPaths.
-- For each level of the tree, it generates the leaves and the nodes, before going to the next level.
-- The possibilities generated are either:
-- - to stop (end of the trajectory)
-- - go straight (no collision)
-- - bump into itself.
-- In the case of a bump, the other "self" can come from different directions.
-- The tree is valid for a given universe only if one trajectory comes back to the bump location (e.g. goign through a time travel portal).
allTrees' :: Omega Tree
allTrees' = pure Stop 
        <|> (Straight    <$> allTrees')
        <|> (Bump Front  <$> allTrees' <*> allTrees')
        <|> (Bump Right_ <$> allTrees' <*> allTrees')
        <|> (Bump Left_  <$> allTrees' <*> allTrees')

-- Move one step in the universe given.
move :: Univ -> Walker -> Walker
move u p = case find (\(Portal a _) -> a == p) u of 
                   Just (Portal _ p) -> p
                   Nothing -> simpleMove p

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


updateHead _ []       = []
updateHead f (a : as) = f a : as

-- sample searchs

search1 :: [Path]
search1 = search initPos portal1 6

pretty1 :: String
pretty1 = concatMap (prettyUnivPath lims portal1) search1

pretty1' :: [String]
pretty1' = map (\t -> prettyUnivPath lims portal1 $ filterTime (head search1) t) [0..] 

filterTime :: Path -> Time -> Path
filterTime ps t = filter (\(PTD _ t' _) -> t == t') ps

