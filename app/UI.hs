{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}

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
import TimeLoop.Types (Univ(emitters))

data ItemType = EntryPortal | ExitPortal | Entry | Exit | Walker
  deriving (Eq, Ord, Show)

data Item = Item {
  itemType :: ItemType,
  time :: Time,
  dir :: Dir,
  sel :: Maybe Bool,
  high :: Maybe Bool}
  deriving (Eq, Ord, Show)

type ItemMap = M.Map Pos [Item]

data SelItem = SelItem {
  itemType  :: ItemType,
  itemIndex :: Int}
  deriving (Eq, Show)

type Step = Int

data UI = UI {
  initUniv :: Univ,
  selItem  :: SelItem, -- Which item is selected
  stepItem :: Step}   -- Which time step is highlighted
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
  , appAttrMap      = const theMap
  }

lims :: Limits
lims = ((-1, -3), (7, 3))


-- * UI

-- Display the whole interface
drawUI :: UI -> [Widget ()]
drawUI (UI u sel st)= [center (border $ drawConfigPanel u sel)
                       <=> drawSearchPanel u st]

-- Display the top panel for configuring the universe.
drawConfigPanel :: Univ -> SelItem -> Widget ()
drawConfigPanel u sel = drawItemMap (getItemsUniv u (Just sel) Nothing) lims 

-- Display the various solutions
drawSearchPanel :: Univ -> Step -> Widget ()
drawSearchPanel u st = hBox $ map drawBlock $ getValidSTBlocks u where
  drawBlock block = border $ drawItemMap (getItemMap block Nothing (Just st)) lims

-- Get the various items in a Block as a Map
getItemMap :: STBlock -> Maybe SelItem -> Maybe Step -> ItemMap
getItemMap (STBlock u ws) sel st = M.map (sortBy $ timePrio st) $ M.unionWith (++) (getItemsUniv u sel st) walkers where
  walkers = M.fromListWith (++) $ map (\(PTD p t d) -> (p, [Item Walker t d Nothing (highlighted t st)])) ws 

-- Get the various items in Univ 
getItemsUniv :: Univ -> Maybe SelItem -> Maybe Step -> ItemMap
getItemsUniv (Univ ps es cs) sel st = M.fromListWith (++) (entries ++ exits ++ portalEntries ++ portalExits) where
  entries       = map (\(PTD p t d)            -> (p, [Item Entry       t d (selected sel Entry)       (highlighted t st)])) es
  exits         = map (\(PTD p t d)            -> (p, [Item Exit        t d (selected sel Exit)        (highlighted t st)])) cs
  portalEntries = map (\(Portal (PTD p t d) _) -> (p, [Item EntryPortal t d (selected sel EntryPortal) (highlighted t st)])) ps
  portalExits   = map (\(Portal _ (PTD p t d)) -> (p, [Item ExitPortal  t d (selected sel ExitPortal)  (highlighted t st)])) ps

highlighted t (Just st') = Just $ (st' `mod` maxStep) == t
highlighted _ _ = Nothing 
selected (Just (SelItem it index)) it' = Just $ it == it'
selected _ _ = Nothing 
  
timePrio (Just st) (Item _ t1 _ _ _) _ | t1 == st `mod` maxStep = LT 
timePrio (Just st) _ (Item _ t2 _ _ _) | t2 == st `mod` maxStep = GT 
timePrio _ a b = compare a b

-- Draws items
drawItemMap :: ItemMap -> Limits ->  Widget ()
drawItemMap is ((minX, minY), (maxX, maxY)) = vBox $ map row [maxY, maxY-1 .. minY] where
  row y = hBox $ map (\x -> drawItems (Pos x y) is) [minX..maxX]

-- Draw items at a specific position
drawItems :: Pos -> ItemMap -> Widget ()
drawItems p is = case M.lookup p is of
  Just items -> drawTile items
  Nothing    -> drawTile [] 

-- draw a single tile
-- Items are sorted by priority: EntryPortal, ExitPortal, Entry, Exit, Walker 
-- Items with the same time than the current step get better priority
-- Only the first will be displayed
drawTile :: [Item] -> Widget ()
drawTile []                                                                      = str tileEmpty 
drawTile ((Item EntryPortal t d sel high) : _)                                   = selectAttr sel $ dimAttr high $ str $ tilePortal True d t 
drawTile ((Item ExitPortal t d sel high) : _)                                    = selectAttr sel $ dimAttr high $ str $ tilePortal False d t 
drawTile ((Item Entry t d sel high) : _)                                         = selectAttr sel $ dimAttr high $ str $ tileEntry d t
drawTile ((Item Exit t d sel high) : _)                                          = selectAttr sel $ dimAttr high $ str $ tileExit d t
drawTile ((Item Walker t1 d1 sel high) : (Item Walker t2 d2 _ _) : _) | t1 == t2 = selectAttr sel $ dimAttr high $ str $ tileCollision d1 d2 t1
drawTile ((Item Walker t d sel high) : _)                                        = selectAttr sel $ dimAttr high $ str $ tileWalker d t

dimAttr, selectAttr :: Maybe Bool -> Widget () -> Widget ()
dimAttr (Just False) = withAttr dimA
dimAttr _ = id 
selectAttr (Just True) = withAttr selA 
selectAttr _ = id


-- * Events

handleEvent  :: BrickEvent () Tick -> EventM () UI ()
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent (VtyEvent (V.EvKey V.KEsc        [])) = halt
handleEvent (VtyEvent (V.EvKey V.KRight      [])) = modify $ move' E
handleEvent (VtyEvent (V.EvKey V.KLeft       [])) = modify $ move' W
handleEvent (VtyEvent (V.EvKey V.KUp         [])) = modify $ move' N 
handleEvent (VtyEvent (V.EvKey V.KDown       [])) = modify $ move' S 
handleEvent (VtyEvent (V.EvKey (V.KChar 'r') [])) = modify rotate 
handleEvent (VtyEvent (V.EvKey (V.KChar '+') [])) = modify $ changeTime True 
handleEvent (VtyEvent (V.EvKey (V.KChar '-') [])) = modify $ changeTime False
handleEvent (VtyEvent (V.EvKey (V.KChar ' ') [])) = modify changePortal
handleEvent (AppEvent Tick                      ) = modify increaseStep
handleEvent _ = return ()


move' :: Dir -> UI -> UI
move' d = updateUI (movePos d)

movePos :: Dir -> PTD -> PTD
movePos N (PTD (Pos x y) t d) = PTD (Pos x (y+1)) t d 
movePos S (PTD (Pos x y) t d) = PTD (Pos x (y-1)) t d 
movePos E (PTD (Pos x y) t d) = PTD (Pos (x+1) y) t d 
movePos W (PTD (Pos x y) t d) = PTD (Pos (x-1) y) t d 

rotate :: UI -> UI
rotate = updateUI (turn Right_)

changeTime :: Bool -> UI -> UI
changeTime b = updateUI (changeTime' b)

changeTime' :: Bool -> PTD -> PTD
changeTime' True  (PTD p t d) = PTD p (t+1) d
changeTime' False (PTD p t d) = PTD p (t-1) d

changePortal :: UI -> UI
changePortal ui@(UI u s _) = ui {selItem = dropWhile (/=s) (getSels u ++ [head $ getSels u]) !! 1}

getSels :: Univ -> [SelItem]
getSels (Univ ps es cs) = portals ++ entries ++ exits where
  portals = concatMap (\i -> [SelItem EntryPortal i, SelItem ExitPortal i]) [0..length ps-1]
  entries = map (SelItem Entry) [0..length es-1]
  exits = map (SelItem Exit) [0..length cs-1]

updateUI :: (PTD -> PTD) -> UI -> UI
updateUI f ui@(UI _ (SelItem EntryPortal i) _) = over (#initUniv % #portals % ix i % #entry) f ui
updateUI f ui@(UI _ (SelItem ExitPortal i) _)  = over (#initUniv % #portals % ix i % #exit)  f ui
updateUI f ui@(UI _ (SelItem Entry i) _)       = over (#initUniv % #emitters % ix i) f ui
updateUI f ui@(UI _ (SelItem Exit i) _)        = over (#initUniv % #consumers % ix i) f ui
updateUI f ui = ui

increaseStep :: UI -> UI
increaseStep (UI ps s st)  = UI ps s (st+1)


-- * Attributes

dimA, selA :: AttrName
dimA   = attrName "Dim"
selA   = attrName "Sel"

theMap :: AttrMap
theMap = attrMap
  V.defAttr
  [(dimA, VA.withStyle VA.defAttr VA.dim),
   (selA, VA.withForeColor VA.defAttr VA.yellow)
  ]

