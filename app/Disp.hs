{-# LANGUAGE OverloadedStrings #-}

module Disp where

import Brick
import Brick.Widgets.Table
import Brick.Widgets.Center (center)
import Tree2
import Data.List.Split

ui :: Widget ()
ui = center $ renderTable $ prettyTab "        \n\n\n" lims ((showPos $ head search1) ++ (showUniv portal1))

prettyTab :: String -> Limits -> [(Int, Int, String)] -> Table ()
prettyTab def ((minX, minY), (maxX, maxY)) ps = setDefaultColAlignment AlignCenter $ setDefaultRowAlignment AlignMiddle $ table $ 
                                                reverse $ chunksOf (maxX - minX +1) $ map (padRight Max . vLimit 5 . hLimit 10 . str) $ strings
 where
  strings :: [String]
  strings = [getString ps def (x, y) | y <- [minY..maxY], x <- [minX..maxX]]

ui' :: Widget ()
ui' = center $ renderTable $ alignCenter 0 $ alignCenter 1 $ alignMiddle 0 $ alignMiddle 1 $ ta

cell :: Widget ()
cell = vLimit 5 $ hLimit 10 $ padRight Max $ padLeft Max $ padBottom Max $ str "A"

centerCell :: Widget n -> Widget n
centerCell = hLimit 10 . vLimit 5 . center

ta :: Table ()
ta = table [[emptyWidget, centerCell $ str "B"],
            [centerCell $ str "longbbbbbbb\nbbbbb", emptyWidget]]
