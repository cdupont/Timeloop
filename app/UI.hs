{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}

module UI where

import Brick
import Brick.Widgets.Table
import Brick.Widgets.Center (hCenter, center)
import Brick.Widgets.Border
import Brick.AttrMap
import TimeLoop.Types
import TimeLoop.Search
import TimeLoop.Walker
import Data.List.Split
import Data.List
import qualified Data.Map as M
import qualified Graphics.Vty as V
import qualified Graphics.Vty.Attributes as VA
import Text.Printf
import Optics
import Optics.Label
import GHC.Generics (Generic)
import Tile

data ItemType = EntryPortal | ExitPortal | Entry | Exit | Walker
  deriving (Eq, Ord, Show)

data Item = Item ItemType Time Dir
  deriving (Eq, Ord, Show)

type ItemMap = M.Map Pos [Item]

data SelItem = SelItem {
  itemType  :: ItemType,
  itemIndex :: Int}

type Step = Int

data UI = UI {
  initUniv :: Univ,
  selItem :: SelItem,
  step :: Step}
  deriving (Generic)

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

lims :: Limits
lims = ((-1, -3), (7, 3))


-- * UI

-- Display the whole interface
drawUI :: UI -> [Widget ()]
drawUI (UI u sel st)= [(center $ border $ drawConfigPanel u sel)
                       <=> drawSearchPanel u st]

-- Display the top panel for configuring the universe.
drawConfigPanel :: Univ -> SelItem -> Widget ()
drawConfigPanel u sel = drawItemMap (getItemsUniv u) lims (Just sel) Nothing 

-- Display the various solutions
drawSearchPanel :: Univ -> Step -> Widget ()
drawSearchPanel u st = hBox $ map drawBlock $ getValidSTBlocks u where
  drawBlock block = border $ drawItemMap (getItemMap block) lims Nothing (Just st)

-- Get the various items in a Block as a Map
getItemMap :: STBlock -> ItemMap
getItemMap (STBlock u ws) = M.unionWith (++) (getItemsUniv u) walkers where
  walkers = M.fromListWith (++) $ map (\(PTD p t d) -> (p, [Item Walker t d])) ws 

-- Get the various items in Univ 
getItemsUniv :: Univ -> ItemMap
getItemsUniv (Univ ps es cs) = M.fromListWith (++) (entries ++ exits ++ portalEntries ++ portalExits) where
  entries       = map (\(PTD p t d)            -> (p, [Item Entry t d])) es
  exits         = map (\(PTD p t d)            -> (p, [Item Exit t d])) cs
  portalEntries = map (\(Portal (PTD p t d) _) -> (p, [Item EntryPortal t d])) ps
  portalExits   = map (\(Portal _ (PTD p t d)) -> (p, [Item ExitPortal t d])) ps

-- Draws items
drawItemMap :: ItemMap -> Limits -> Maybe SelItem -> Maybe Step ->  Widget ()
drawItemMap is ((minX, minY), (maxX, maxY)) sel st = vBox $ map row [maxY, maxY-1 .. minY] where
  row y = hBox $ map (\x -> drawItems (Pos x y) is sel st) [minX..maxX]

-- Draw items at a specific position
drawItems :: Pos -> ItemMap -> Maybe SelItem -> Maybe Step -> Widget ()
drawItems p is sel st = case M.lookup p is of
  Just items -> drawTile items (getItemType sel) st
  Nothing    -> drawTile [] Nothing Nothing 
 where
   getItemType (Just (SelItem it _)) = Just it
   getItemType Nothing = Nothing

-- draw a single tile
-- Items are sorted by priority: EntryPortal, ExitPortal, Entry, Exit, Walker 
-- Items with the same time than the current step get better priority
-- Only the first will be displayed
drawTile :: [Item] -> Maybe ItemType -> Maybe Step -> Widget ()
drawTile is sel st = drawTile' (sortBy (timePrio st) is) st where
  timePrio (Just st) (Item _ t1 _) (Item _ t2 _) | (t1 == st `mod` maxStep) = LT 
  timePrio (Just st) (Item _ t1 _) (Item _ t2 _) | (t2 == st `mod` maxStep) = GT 
  timePrio _ a b = compare a b
  --selectAttr = if (Just it) == sel then withAttr red else id

drawTile' :: [Item] -> Maybe Step -> Widget ()
drawTile' [] _                                                          = str tileEmpty 
drawTile' ((Item EntryPortal t d) : _) st                               = dimAttr st t $ str $ tilePortal True d t 
drawTile' ((Item ExitPortal t d) : _) st                                = dimAttr st t $ str $ tilePortal False d t 
drawTile' ((Item Entry t d) : _) st                                     = dimAttr st t $ str $ tileWalker d t
drawTile' ((Item Exit t d) : _) st                                      = dimAttr st t $ str $ tileWalker d t
drawTile' ((Item Walker t1 d1) : (Item Walker t2 d2) : _) st | t1 == t2 = dimAttr st t1 $ str $ tileCollision d1 d2 t1
drawTile' ((Item Walker t d) : _) st                                    = dimAttr st t $ str $ tileWalker d t

dimAttr :: Maybe Step -> Time -> Widget () -> Widget ()
dimAttr (Just st') t = if (st' `mod` maxStep) /= t then withAttr dim else id
dimAttr Nothing t = id 


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
move' d ui@(UI _ (SelItem EntryPortal _) _) = over (#initUniv % #portals % mapped % #entry) (movePos d) ui
move' d ui@(UI _ (SelItem ExitPortal _) _)  = over (#initUniv % #portals % mapped % #exit)  (movePos d) ui

movePos :: Dir -> PTD -> PTD
movePos N (PTD (Pos x y) t d) = PTD (Pos x (y+1)) t d 
movePos S (PTD (Pos x y) t d) = PTD (Pos x (y-1)) t d 
movePos E (PTD (Pos x y) t d) = PTD (Pos (x+1) y) t d 
movePos W (PTD (Pos x y) t d) = PTD (Pos (x-1) y) t d 

rotate :: UI -> UI
rotate ui@(UI _ (SelItem EntryPortal _ ) _) = over (#initUniv % #portals % mapped % #entry) (turn Right_) ui
rotate ui@(UI _ (SelItem ExitPortal _ ) _)  = over (#initUniv % #portals % mapped % #exit) (turn Right_) ui


changeTime :: Bool -> UI -> UI
changeTime b ui@(UI _ (SelItem EntryPortal _) _) = over (#initUniv % #portals % mapped % #entry) (changeTime' b) ui
changeTime b ui@(UI _ (SelItem ExitPortal _) _)  = over (#initUniv % #portals % mapped % #exit) (changeTime' b) ui

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
red   = attrName "Red"

theMap :: AttrMap
theMap = attrMap
  V.defAttr
  [(blink, VA.withStyle VA.defAttr VA.blink),
   (dim, VA.withStyle VA.defAttr VA.dim),
   (red, VA.withForeColor VA.defAttr VA.red)
  ]

