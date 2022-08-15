{-# LANGUAGE OverloadedStrings #-}

module UI where

import Brick
import Brick.Widgets.Table
import Brick.Widgets.Center (hCenter, center)
import TimeLoop.Types
import TimeLoop.Search
import TimeLoop.Pretty
import Data.List.Split

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
