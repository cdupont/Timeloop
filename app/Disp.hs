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
                                                reverse $ chunksOf (maxX - minX +1) $ map str $ strings
 where
  strings :: [String]
  strings = [getString ps def (x, y) | y <- [minY..maxY], x <- [minX..maxX]]


