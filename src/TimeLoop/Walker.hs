{-# LANGUAGE MonadComprehensions #-}

module TimeLoop.Walker where

import TimeLoop.Types


-- move one or several walkers that are at the same point in spacetime
-- in case of collision, we always turn right
move :: [Walker] -> [Walker]
move [w] = [simpleMove w]
move ws = map (simpleMove . turn Right_) ws

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
turn' Right_ W = N
turn' Right_ d = succ d
turn' Left_ N  = W 
turn' Left_ d  = pred d 
turn' Back a   = turn' Right_ $ turn' Right_ a
turn' Front a  = a

