{-# LANGUAGE OverloadedStrings #-}

module UI where

import Brick
import Brick.Widgets.Table
import Brick.Widgets.Center (center)
import TimeLoop.Types
import TimeLoop.Search
import TimeLoop.Pretty
import Data.List.Split

tableDisplay :: Widget ()
tableDisplay = center $ renderTable $ prettyTab ((showPos $ head search1) ++ (showUniv portal1)) "        \n\n\n" lims 

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
