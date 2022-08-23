{-# LANGUAGE OverloadedStrings #-}

module UI where

import Brick
import Brick.Widgets.Table
import Brick.Widgets.Center (hCenter, center)
import Brick.Widgets.Border
import Brick.AttrMap
import TimeLoop.Types
import TimeLoop.Search
import TimeLoop.Pretty
import Data.List.Split
import Data.List
import qualified Data.Map as M
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Attributes as VA

data ItemType = EntryPortal | ExitPortal | Walker
  deriving Eq

data Item = Item ItemType Time Dir

type ItemMap = M.Map Pos [Item]

data SelItem = SelItem {
  itemType :: ItemType,
  itemIndex :: Int}

type Step = Int

data UI = UI {
  univ :: Univ,
  selItem :: SelItem,
  step :: Step}

-- | Ticks mark passing of time
data Tick = Tick


-- * Main app

app :: App UI Tick ()
app = App
  { appDraw         = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = return ()
  , appAttrMap      = const $ theMap
  }

-- * UI

-- Display the whole interface
drawUI :: UI -> [Widget ()]
drawUI (UI u sel st)= singleton $ (center $ border $ drawItems (getItemsUniv u) lims (Just sel) st)
                                  <=> drawSearchPanel u st

-- Display the various solutions
drawSearchPanel :: Univ -> Step -> Widget ()
drawSearchPanel u st = hBox $ map drawPath paths where
  drawPath path = border $ drawItems (getItemsPath path) lims Nothing st
  paths = take 3 $ search initPos u 6

getItemsUniv :: Univ -> ItemMap
getItemsUniv ps = M.unionsWith (<>) (concatMap (\(Portal (PTD p1 t1 d1) (PTD p2 t2 d2)) -> [ M.singleton p1 [Item EntryPortal t1 d1], 
                                                                                           M.singleton p2 [Item ExitPortal t2 d2]]) ps)

getItemsPath :: Path -> ItemMap
getItemsPath p = M.fromList $ map (\(PTD p t d) -> (p, [Item Walker t d])) p

drawItems :: ItemMap -> Limits -> Maybe SelItem -> Step ->  Widget ()
drawItems is ((minX, minY), (maxX, maxY)) sel st = vBox $ map row [maxY, maxY-1 .. minY] where
  row y = hBox $ map (\x -> getWidget (Pos x y) is sel st) [minX..maxX]

getWidget :: Pos -> ItemMap -> Maybe SelItem -> Step -> Widget ()
getWidget p is sel st = case M.lookup p is of
  Just (item : _) -> drawItem item (getItemType sel) st
  Nothing         -> emptyCell
 where
   getItemType (Just (SelItem it _)) = Just it
   getItemType Nothing = Nothing

drawItem :: Item -> Maybe ItemType -> Step -> Widget ()
drawItem (Item it t d) sel st = selectAttr $ drawItem' (Item it t d) st where
  selectAttr = if (Just it) == sel then withAttr blink else id

drawItem' :: Item -> Step -> Widget ()
drawItem' (Item EntryPortal t d) _ = border $ (str $ show d ++ "□ " ++ show t) 
                                     <=> (str "    ")
drawItem' (Item ExitPortal t d) _ = border $ (str $ show d ++ "▣ " ++ show t)
                                     <=> (str "    ") 
drawItem' (Item Walker t d) st  = dimAttr st t $
                                     (str "     ") 
                                 <=> (str $ show d ++ show t) 
                                 <=> (str "     ") where
  dimAttr st t = if (st `mod` 6) /= t then withAttr dim else id

emptyCell :: Widget ()
emptyCell = str "       " 
        <=> str "       " 
        <=> str "       " 
        <=> str "       " 


-- * Events

handleEvent  :: BrickEvent () Tick -> EventM () UI ()
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
handleEvent (AppEvent Tick                      ) = modify $ increaseStep
handleEvent _ = return ()


move' :: Dir -> UI -> UI
move' d ui@(UI [(Portal p1 p2)] (SelItem EntryPortal _) _) = ui { univ = [Portal (movePos d p1) p2]}
move' d ui@(UI [(Portal p1 p2)] (SelItem ExitPortal _) _)  = ui { univ = [Portal p1 (movePos d p2)]}

movePos :: Dir -> PTD -> PTD
movePos N (PTD (Pos x y) t d) = PTD (Pos x (y+1)) t d 
movePos S (PTD (Pos x y) t d) = PTD (Pos x (y-1)) t d 
movePos E (PTD (Pos x y) t d) = PTD (Pos (x+1) y) t d 
movePos W (PTD (Pos x y) t d) = PTD (Pos (x-1) y) t d 

rotate :: UI -> UI
rotate ui@(UI [(Portal p1 p2)] (SelItem EntryPortal _ ) _) = ui { univ = [Portal (turn Right_ p1) p2]}
rotate ui@(UI [(Portal p1 p2)] (SelItem ExitPortal _ ) _)  = ui { univ = [Portal p1 (turn Right_ p2)]}

changeTime :: Bool -> UI -> UI
changeTime b ui@(UI [(Portal p1 p2)] (SelItem EntryPortal _) _) = ui { univ = [Portal (changeTime' b p1) p2]}
changeTime b ui@(UI [(Portal p1 p2)] (SelItem ExitPortal _) _)  = ui { univ = [Portal p1 (changeTime' b p2)]}

changeTime' :: Bool -> PTD -> PTD
changeTime' True  (PTD p t d) = PTD p (t+1) d
changeTime' False (PTD p t d) = PTD p (t-1) d

changePortal :: UI -> UI
changePortal ui@(UI _ (SelItem EntryPortal i) _) = ui {selItem = SelItem ExitPortal i}
changePortal ui@(UI _ (SelItem ExitPortal i) _)  = ui {selItem = SelItem EntryPortal i}

increaseStep :: UI -> UI
increaseStep (UI ps s st)  = UI ps s (st+1)

-- * Attributes

blink, dim :: AttrName
blink = attrName "Blink"
dim   = attrName "Dim"

theMap :: AttrMap
theMap = attrMap
  V.defAttr
  [(blink, VA.withStyle VA.defAttr VA.blink),
   (dim, VA.withStyle VA.defAttr VA.dim)
  ]

