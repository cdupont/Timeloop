
module Tile where

import TimeLoop.Types
import TimeLoop.Walker
import Text.Printf

tilePortal :: Bool -> Dir -> Time -> String
tilePortal in_ dir time =  
  "┌─" ++ n ++ "─┐\n" ++
   w ++   c  ++ e ++ "\n" ++
  "└─" ++ s ++ "─┘" where
  w = if side == W then showArr dir else "│ " 
  e = if side == E then showArr dir else " │" 
  n = if side == N then showArr dir else "──" 
  s = if side == S then showArr dir else "──" 
  c = printf "%2d" time
  side = if in_ then turn' Back dir else dir 

tileWalker :: Dir -> Time -> String
tileWalker = tileArr showArr  

tileEntry :: Dir -> Time -> String
tileEntry = tileArr showFromBarArr  

tileExit :: Dir -> Time -> String
tileExit = tileArr showToBarArr  

tileArr :: (Dir -> String) -> Dir -> Time -> String
tileArr showArr dir time = 
  "    " ++ t ++ "\n" ++
  "  " ++ c ++ "  \n" ++
  "      \n" where
  c = showArr dir 
  t = printf "%2d" time

tileCollision :: Dir -> Dir -> Time -> String
tileCollision d1 d2 time =
  "  " ++ n ++ t ++"\n" ++
   w ++  "★ "  ++ e ++ "\n" ++
  "  " ++ s ++ "  " where
  w = getArrLoc E (d1, d2)
  e = getArrLoc W (d1, d2)
  s = getArrLoc N (d1, d2)
  n = getArrLoc S (d1, d2)
  t = printf "%2d" time

tileEmpty :: String 
tileEmpty =  "      \n" ++
             "      \n" ++ 
             "      " 

showArr :: Dir -> String
showArr N = "↑ "
showArr W = "← "
showArr E = "→ "
showArr S = "↓ "

showFromBarArr :: Dir -> String
showFromBarArr N = "↥ "
showFromBarArr W = "↤ "
showFromBarArr E = "↦ "
showFromBarArr S = "↧ "

showToBarArr :: Dir -> String
showToBarArr N = "⤒ "
showToBarArr W = "⇤ "
showToBarArr E = "⇥ "
showToBarArr S = "⤓ "


-- Get the collision arrow given your own direction 
getArrLoc :: Dir -> (Dir, Dir) -> String
getArrLoc d (d1, d2)  | d == d1 = getAngleArr (d, turn' Right_ d) 
getArrLoc d (d1, d2)  | d == d2 = getAngleArr (d, turn' Right_ d) 
getArrLoc _  _ = "  "

getAngleArr :: (Dir, Dir) -> String
getAngleArr (N, E) = "↱ "
getAngleArr (N, S) = "↱ "
getAngleArr (N, W) = "↰ "
getAngleArr (S, E) = "↳ "
getAngleArr (S, W) = "↲ "
getAngleArr (S, N) = "↲ "
getAngleArr (E, N) = "⬏ "
getAngleArr (E, S) = "⬎ "
getAngleArr (E, W) = "⬎ "
getAngleArr (W, N) = "⬑ "
getAngleArr (W, E) = "⬑ "
getAngleArr (W, S) = "⬐ "
getAngleArr (_, _) = "  "

