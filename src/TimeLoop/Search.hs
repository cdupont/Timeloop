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

move :: Univ -> Pos -> Pos
move u p = case find (\(Portal a _) -> a == p) u of 
                   Just (Portal _ p) -> p
                   Nothing -> simpleMove p

simpleMove :: Pos -> Pos
simpleMove (Pos x y t N) = Pos x (y+1) (t+1) N
simpleMove (Pos x y t S) = Pos x (y-1) (t+1) S
simpleMove (Pos x y t E) = Pos (x+1) y (t+1) E
simpleMove (Pos x y t W) = Pos (x-1) y (t+1) W

turn :: RelDir -> Pos -> Pos
turn rd (Pos x y t d) = Pos x y t (turn' rd d)

turn' :: RelDir -> Dir -> Dir
turn' Right_ N = E 
turn' Right_ E = S 
turn' Right_ S = W 
turn' Right_ W = N
turn' Back a   = turn' Right_ $ turn' Right_ a
turn' Left_ a  = turn' Right_ $ turn' Back a
turn' Front a  = a

getPaths :: Tree -> Pos -> Univ -> [[Pos]]
getPaths (Straight t) p u      =  updateHead (p :) (getPaths t  (move u p) u)
getPaths (Bump from t1 t2) p u = (updateHead (p :) (getPaths t1 (move u (turn Right_ p)) u)) ++
                                 (updateHead (p':) (getPaths t2 (move u (turn Right_ p')) u)) where
                                 p' = turn Back $ turn from p
getPaths (Loop i) p u = [[p]] 
getPaths Stop p u = [[p]]

getPaths' :: Pos -> Univ -> Tree -> [[Pos]]
getPaths' p u t = getPaths t p u

allTrees :: [Tree]
allTrees = runOmega allTrees'

allTrees' :: Omega Tree
allTrees' = pure Stop 
       <|> Straight    <$> allTrees'
       <|> Bump Front  <$> allTrees' <*> allTrees'
       <|> Bump Right_ <$> allTrees' <*> allTrees'
       <|> Bump Left_  <$> allTrees' <*> allTrees'


allPaths :: Pos -> Univ -> [Path]
allPaths p u = join $ map chains $ map (getPaths' p u) allTrees

chains :: [Path] -> [Path]
chains ps = catMaybes $ map joinPaths $ permutations ps 

joinPaths :: [Path] -> Maybe Path
joinPaths ps = loop ps 
  where
    loop []       = Nothing 
    loop [a]      = Just a
    loop (x:y:zs) = if (last x == head y) 
                    then loop ((x ++ (tail y)) : zs)
                    else Nothing

updateHead _ []       = []
updateHead f (a : as) = f a : as

search :: Univ -> Int -> [Path]
search u depth = filter (\l -> length l == depth) $ allPaths initPos u

-- sample searchs

search1 :: [Path]
search1 = filter (\l -> length l == 6) $ allPaths initPos portal1

pretty1 :: String
pretty1 = concatMap (prettyUnivPath lims portal1) search1

pretty1' :: [String]
pretty1' = map (\t -> prettyUnivPath lims portal1 $ filterTime (head search1) t) [0..] 

filterTime :: Path -> Time -> Path
filterTime ps t = filter (\(Pos _ _ t' _) -> t == t') ps

stepper :: IO ()
stepper = forM_ pretty1' (\s -> do
  getChar
  putStr "\ESC[2J"
  putStrLn s)
  

