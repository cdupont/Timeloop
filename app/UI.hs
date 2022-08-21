{-# LANGUAGE OverloadedStrings #-}

module UI where

import Brick
import Brick.Widgets.Table
import Brick.Widgets.Center (hCenter, center)
import Brick.Widgets.Border
import TimeLoop.Types
import TimeLoop.Search
import TimeLoop.Pretty
import Data.List.Split
import Data.List
import qualified Data.Map as M
import qualified Graphics.Vty as V

data ItemType = EntryPortal | ExitPortal | Walker

data Item = Item ItemType Time Dir

type ItemMap = M.Map Pos [Item]

data UI = UI {
  univ :: Univ,
  portalIndex :: Int,
  portalType :: ItemType}

-- * Main app

app :: App UI () ()
app = App
  { appDraw         = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = return ()
  , appAttrMap      = const $ attrMap V.defAttr []
  }

-- * UI

-- Display the whole interface
drawUI :: UI -> [Widget ()]
drawUI (UI u _ _)= [drawUI' u]

drawUI' :: Univ -> Widget ()
drawUI' u = (center $ border $ drawItems (getItemsUniv u) lims)
         <=> drawSearchPanel u

-- Display the various solutions
drawSearchPanel :: Univ -> Widget ()
drawSearchPanel u = hBox $ map drawPath paths where
  drawPath path = border $ drawItems (getItemsPath path) lims 
  paths = take 3 $ search initPos u 6

getItemsUniv :: Univ -> ItemMap
getItemsUniv ps = M.unionsWith (<>) (concatMap (\(Portal (PTD p1 t1 d1) (PTD p2 t2 d2)) -> [ M.singleton p1 [Item EntryPortal t1 d1], 
                                                                                           M.singleton p2 [Item ExitPortal t2 d2]]) ps)

getItemsPath :: Path -> ItemMap
getItemsPath p = M.fromList $ map (\(PTD p t d) -> (p, [Item Walker t d])) p

drawItems :: ItemMap -> Limits -> Widget ()
drawItems is ((minX, minY), (maxX, maxY)) = vBox $ map row [maxY, maxY-1 .. minY] where
  row y = hBox $ map (\x -> getWidget (Pos x y) is) [minX..maxX]

getWidget :: Pos -> ItemMap -> Widget ()
getWidget p is = case M.lookup p is of
  Just (item : _) -> drawItem item
  Nothing         -> emptyCell

drawItem :: Item -> Widget ()
drawItem (Item EntryPortal t d) = border $ (str $ show d ++ "□ " ++ show t) 
                                     <=> (str "    ")
drawItem (Item ExitPortal t d)  = border $ (str $ show d ++ "▣ " ++ show t)
                                     <=> (str "    ") 
drawItem (Item Walker t d)      = (str "     ") 
                                <=> (str $ show d ++ show t) 
                                <=> (str "     ")

emptyCell :: Widget ()
emptyCell = str "       " 
        <=> str "       " 
        <=> str "       " 
        <=> str "       " 


-- * Events

handleEvent  :: BrickEvent () () -> EventM () UI ()
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent (VtyEvent (V.EvKey V.KEsc        [])) = halt
handleEvent (VtyEvent (V.EvKey V.KRight      [])) = modify $ move' E
handleEvent (VtyEvent (V.EvKey V.KLeft       [])) = modify $ move' W
handleEvent (VtyEvent (V.EvKey V.KUp         [])) = modify $ move' N 
handleEvent (VtyEvent (V.EvKey V.KDown       [])) = modify $ move' S 
handleEvent (VtyEvent (V.EvKey (V.KChar 'r') [])) = modify $ rotate 
handleEvent (VtyEvent (V.EvKey (V.KChar '+') [])) = modify $ changeTime True 
handleEvent (VtyEvent (V.EvKey (V.KChar '-') [])) = modify $ changeTime False
handleEvent (VtyEvent (V.EvKey (V.KChar ' ') [])) = modify $ changePortal
handleEvent _ = return ()


move' :: Dir -> UI -> UI
move' d (UI [(Portal p1 p2)] index EntryPortal) = UI [Portal (movePos d p1) p2] index EntryPortal 
move' d (UI [(Portal p1 p2)] index ExitPortal)  = UI [Portal p1 (movePos d p2)] index ExitPortal

movePos :: Dir -> PTD -> PTD
movePos N (PTD (Pos x y) t d) = PTD (Pos x (y+1)) t d 
movePos S (PTD (Pos x y) t d) = PTD (Pos x (y-1)) t d 
movePos E (PTD (Pos x y) t d) = PTD (Pos (x+1) y) t d 
movePos W (PTD (Pos x y) t d) = PTD (Pos (x-1) y) t d 

rotate :: UI -> UI
rotate (UI [(Portal p1 p2)] index EntryPortal) = UI [Portal (turn Right_ p1) p2] index EntryPortal
rotate (UI [(Portal p1 p2)] index ExitPortal)  = UI [Portal p1 (turn Right_ p2)] index ExitPortal

changeTime :: Bool -> UI -> UI
changeTime b (UI [(Portal p1 p2)] index EntryPortal) = UI [Portal (changeTime' b p1) p2] index EntryPortal 
changeTime b (UI [(Portal p1 p2)] index ExitPortal)  = UI [Portal p1 (changeTime' b p2)] index ExitPortal

changeTime' :: Bool -> PTD -> PTD
changeTime' True  (PTD p t d) = PTD p (t+1) d
changeTime' False (PTD p t d) = PTD p (t-1) d

changePortal :: UI -> UI
changePortal (UI ps i EntryPortal) = UI ps i ExitPortal
changePortal (UI ps i ExitPortal)  = UI ps i EntryPortal


-- * Attributes

theMap :: AttrMap
theMap = attrMap
  V.defAttr
  [
  ]

