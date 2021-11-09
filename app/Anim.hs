{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}

module Anim where

import qualified Data.ByteString.Lazy  as BL
import qualified Diagrams as D hiding (Animation)
import           Reanimate
import           Reanimate.Builtin.Documentation
import           Diag
import           Graphics.SvgTree.Types
import qualified Graphics.Svg.Core as Svg
import qualified Diagrams.Backend.SVG  as D
-- import qualified Diagrams.Core.Compile as D
--import qualified Diagrams.Core.Types   as D
import qualified Diagrams.Size         as D
import           Graphics.SvgTree      (Document (..), Tree (..), defaultSvg,
                                        loadSvgFile, parseSvgFile,
                                        xmlOfDocument)
import Data.String.Conversions

anim :: IO ()
--anim = reanimate $ docEnv $ playThenReverseA drawCircle
anim = reanimate $ env $ animate $ const r --((mkCircle 1))

env :: Animation -> Animation
env = mapA $ \svg -> mkGroup
  [ mkBackground "white"
  , withFillOpacity 1 $
    withStrokeWidth 0.1 $
    withStrokeColor "black" (mkGroup [svg]) ]


r :: Tree
r = renderDiagram $ col1 

renderDiagram :: D.Diagram D.SVG -> Tree
renderDiagram d =
    case parseSvgFile "" (convertString $ Svg.renderText (D.renderDia D.SVG opts d)) of
      Nothing  -> error "Malformed svg"
      Just svg -> unbox svg
  where
    -- opts = SVGOptions (mkSizeSpec (V2 Nothing Nothing)) Nothing "" [] False
    opts = D.SVGOptions D.absolute Nothing "" [] False
