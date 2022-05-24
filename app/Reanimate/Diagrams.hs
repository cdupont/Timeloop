{-# LANGUAGE OverloadedStrings #-}
module Reanimate.Diagrams
  ( renderDiagram
  ) where

import Data.Text.Lazy (toStrict)
import Diagrams.Backend.SVG (SVG (SVG), Options (SVGOptions))
import Diagrams.Core.Types (Diagram)
import Diagrams.Core.Compile (renderDia)
import Diagrams.Size (absolute, mkSizeSpec)
import Graphics.Svg.Core (renderText)
import Graphics.SvgTree (Tree, parseSvgFile)
import Reanimate.Svg.Unuse (unbox)
import qualified Reanimate as R
import Reanimate (boundingBox, center, flipYAxis, scale)

import Data.Maybe (maybe)
import Debug.Trace
import Diagrams.TwoD.Size

renderDiagram :: Diagram SVG -> Tree
renderDiagram d =
  let opts = SVGOptions (mkWidth 250) Nothing "" [] False
      svg = renderDia SVG opts d
      textSvg = traceShowId $ toStrict $ renderText svg
  in  case (parseSvgFile "" textSvg) of
        Just tree -> fillCanvas $ flipYAxis $ unbox tree
        Nothing -> error "Malformed SVG"

fillCanvas :: R.SVG -> R.SVG
fillCanvas t = scale m $ center t
 where
  (_, _, w, h) = boundingBox t
  m = min (16/w) (9/h)
