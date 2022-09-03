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
drawConfigPanel u sel = drawItemMap (getItemsUniv u) lims (Just sel) 0

-- Display the various solutions
drawSearchPanel :: Univ -> Step -> Widget ()
drawSearchPanel u st = hBox $ map drawBlock $ getValidSTBlocks u where
  drawBlock block = border $ drawItemMap (getItemMap block) lims Nothing st

-- Get the various items in a block as a Map
getItemMap :: STBlock -> ItemMap
getItemMap (STBlock u ws) = M.unionWith (++) (getItemsUniv u) walkers where
  walkers = M.fromListWith (++) $ map (\(PTD p t d) -> (p, [Item Walker t d])) ws 

getItemsUniv :: Univ -> ItemMap
getItemsUniv (Univ ps es cs) = M.fromListWith (++) (entries ++ exits ++ portalEntries ++ portalExits) where
  entries       = map (\(PTD p t d)            -> (p, [Item Entry t d])) es
  exits         = map (\(PTD p t d)            -> (p, [Item Exit t d])) cs
  portalEntries = map (\(Portal (PTD p t d) _) -> (p, [Item EntryPortal t d])) ps
  portalExits   = map (\(Portal _ (PTD p t d)) -> (p, [Item ExitPortal t d])) ps

-- Draws items
drawItemMap :: ItemMap -> Limits -> Maybe SelItem -> Step ->  Widget ()
drawItemMap is ((minX, minY), (maxX, maxY)) sel st = vBox $ map row [maxY, maxY-1 .. minY] where
  row y = hBox $ map (\x -> drawItems (Pos x y) is sel st) [minX..maxX]

-- Draw items at a specific position
drawItems :: Pos -> ItemMap -> Maybe SelItem -> Step -> Widget ()
drawItems p is sel st = case M.lookup p is of
  Just items -> drawItem items (getItemType sel) st
  Nothing    -> drawItem [] Nothing 0 
 where
   getItemType (Just (SelItem it _)) = Just it
   getItemType Nothing = Nothing

drawItem :: [Item] -> Maybe ItemType -> Step -> Widget ()
drawItem is sel st = drawItem' (sort is) st where
  --selectAttr = if (Just it) == sel then withAttr red else id

drawItem' :: [Item] -> Step -> Widget ()
drawItem' [] _ = str tileEmpty 
drawItem' ((Item EntryPortal t d) : _) _ = str $ tilePortal True d t 
drawItem' ((Item ExitPortal t d) : _) _  = str $ tilePortal False d t 
drawItem' ((Item Entry t d) : _) st      = str $ tileWalker d t
drawItem' ((Item Exit t d) : _) st       = str $ tileWalker d t
drawItem' ((Item Walker t1 d1) : (Item Walker t2 d2) : _) st     = str $ tileCollision d1 d2 t1 where
drawItem' ((Item Walker t d) : _) st     = str $ tileWalker d t
--  dimAttr st t = if (st `mod` 6) /= t then withAttr dim else id

tilePortal :: Bool -> Dir -> Time -> String
tilePortal in_ dir time =  
  "┌─" ++ n ++ "─┐\n" ++
   w ++   c  ++ e ++ "\n" ++
  "└─" ++ s ++ "─┘" where
  w = if side == W then show dir else "│ " 
  e = if side == E then show dir else " │" 
  n = if side == N then show dir else "──" 
  s = if side == S then show dir else "──" 
  c = printf "%2d" time
  side = if in_ then turn' Back dir else dir 


tileWalker :: Dir -> Time -> String
tileWalker dir time = 
  "    " ++ t ++ "\n" ++
  "  " ++ c ++ "  \n" ++
  "      \n" where
  c = show dir 
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

