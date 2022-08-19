{-# LANGUAGE OverloadedStrings #-}

module UI where

import Brick
import Brick.Widgets.Table
import Brick.Widgets.Center (hCenter, center)
import TimeLoop.Types
import TimeLoop.Search
import TimeLoop.Pretty
import Data.List.Split
import qualified Graphics.Vty as V

data PortalType = Entry | Exit

data UI = UI {
  univ :: Univ,
  portalIndex :: Int,
  portalType :: PortalType}

app :: App UI () ()
app = App
  { appDraw         = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = return ()
  , appAttrMap      = const $ attrMap V.defAttr []
  }

drawUI :: UI -> [Widget ()]
drawUI (UI u _ _)= [tableUniv u]

handleEvent  :: BrickEvent () () -> EventM () UI ()
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent (VtyEvent (V.EvKey V.KEsc        [])) = halt
handleEvent (VtyEvent (V.EvKey V.KRight      [])) = movePortal E
handleEvent (VtyEvent (V.EvKey V.KLeft       [])) = movePortal W
handleEvent (VtyEvent (V.EvKey V.KUp         [])) = movePortal N 
handleEvent (VtyEvent (V.EvKey V.KDown       [])) = movePortal S 
handleEvent _ = return ()

movePortal :: Dir -> EventM () UI ()
movePortal d = do  
  ui <- get 
  put $ move' d ui

move' :: Dir -> UI -> UI
move' d (UI [(Portal p1 p2)] index Entry) = UI [Portal (movePos d p1) p2] index Entry 
move' d (UI [(Portal p1 p2)] index Exit)  = UI [Portal p1 (movePos d p2)] index Exit

movePos :: Dir -> PTD -> PTD
movePos N (PTD (Pos x y) t d) = PTD (Pos x (y+1)) t d 
movePos S (PTD (Pos x y) t d) = PTD (Pos x (y-1)) t d 
movePos E (PTD (Pos x y) t d) = PTD (Pos (x+1) y) t d 
movePos W (PTD (Pos x y) t d) = PTD (Pos (x-1) y) t d 

-- Display the whole interface
tableDisplay :: Univ -> Widget ()
tableDisplay u = tableUniv u <=> tableSearch u 

-- Display the universe initial state 
tableUniv :: Univ -> Widget ()
tableUniv u = hCenter $ renderTable $ prettyTab (showUniv u) "        \n\n\n" lims

-- Display the various solutions
tableSearch :: Univ -> Widget ()
tableSearch u = hBox $ map tablePath paths where
  paths = take 3 $ search initPos u 6

-- display a single path
tablePath :: Path -> Widget ()
tablePath path = renderTable $ prettyTab (showPos path) "        \n\n\n" lims 

-- Display in a Table the content of the lookup table
prettyTab :: [(Int, Int, String)] -> String -> Limits -> Table ()
prettyTab items def ((minX, minY), (maxX, maxY)) = table $ reverse $ chunksOf (maxX - minX +1) $ map (centerCell . str) $ strings
 where
  strings :: [String]
  strings = [getString items def (x, y) | y <- [minY..maxY], x <- [minX..maxX]]

centerCell :: Widget n -> Widget n
centerCell = hLimit 7 . vLimit 3 . center

--ui' :: Widget ()
--ui' = center $ renderTable ta
--
--
--ta :: Table ()
--ta = table [[emptyWidget, centerCell $ str "B"],
--            [centerCell $ str "longbbbbbbbbbbbb", emptyWidget]]
