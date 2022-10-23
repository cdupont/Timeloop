{-# LANGUAGE OverloadedLabels #-}

module TimeLoop.Walker where

import TimeLoop.Types
import Optics
import Optics.Label

-- move one or several walkers that are at the same point in spacetime
-- in case of collision, we always turn right
move :: [Walker] -> [Walker]
move [w] = [simpleMove w]
move ws = map (simpleMove . turn Right_) ws

-- Move one step in a flat universe.
simpleMove :: Walker -> Walker
simpleMove w = case view (#unWalker % #dir) w of
  N -> over (#unWalker % #pos % #y) (+1) w'
  S -> over (#unWalker % #pos % #y) (subtract 1) w'
  E -> over (#unWalker % #pos % #x) (+1) w'
  W -> over (#unWalker % #pos % #x) (subtract 1) w' 
  where
  w' = over (#unWalker % #time) (+1) w

-- Turn a walker using a relative direction
turn :: RelDir -> Walker -> Walker
turn rd = over #unWalker (turn' rd)

turn' :: RelDir -> PTD -> PTD 
turn' rd = over #dir (turnRel rd)

-- Turn an absolute direction using a relative one
turnRel :: RelDir -> Dir -> Dir
turnRel Right_ W = N
turnRel Right_ d = succ d
turnRel Left_ N  = W 
turnRel Left_ d  = pred d 
turnRel Back a   = turnRel Right_ $ turnRel Right_ a
turnRel Front a  = a

