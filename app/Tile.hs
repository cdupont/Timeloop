
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
  w = getArr E (d1, d2)
  e = getArr W (d1, d2)
  s = getArr N (d1, d2)
  n = getArr S (d1, d2)
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

getArr :: Dir -> (Dir, Dir) -> String
getArr d (d1, d2) = case getOtherDir d (d1, d2) of
                      Just (da, db) -> getArr' (da, db) 
                      Nothing -> "  "

getOtherDir :: Dir -> (Dir, Dir) -> Maybe (Dir, Dir)
getOtherDir d (d1, d2) | d == d1 = Just (d1, d2)
getOtherDir d (d1, d2) | d == d2 = Just (d2, d1)
getOtherDir _ _ = Nothing
  

getArr' :: (Dir, Dir) -> String
getArr' (N, E) = "↱ "
getArr' (N, S) = "↱ "
getArr' (N, W) = "↰ "
getArr' (S, E) = "↳ "
getArr' (S, W) = "↲ "
getArr' (S, N) = "↲ "
getArr' (E, N) = "⬏ "
getArr' (E, S) = "⬎ "
getArr' (E, W) = "⬎ "
getArr' (W, N) = "⬑ "
getArr' (W, E) = "⬑ "
getArr' (W, S) = "⬐ "
getArr' (_, _) = "  "


