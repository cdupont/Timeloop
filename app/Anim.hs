{-# LANGUAGE ParallelListComp #-}

module Anim where

import qualified Data.ByteString.Lazy  as BL
import           Data.String.Conversions
import           Diag
import           Diagrams.Prelude hiding (boundingBox, Animation, rotate, E, start, scale, center)
import           Diagrams.Backend.SVG.CmdLine hiding (SVG)
import qualified Diagrams.Size         as D
import qualified Diagrams.Backend.SVG  as D
import           Graphics.SvgTree.Types
import qualified Graphics.Svg.Core as Svg
import           Graphics.SvgTree      (Document (..), Tree (..), defaultSvg,
                                        loadSvgFile, parseSvgFile,
                                        xmlOfDocument)
import qualified Reanimate.Diagrams as RA
import           Reanimate
import           Reanimate.Svg.Constructors
import           Reanimate.Builtin.Documentation


anim :: IO ()
anim = do
  f <- loadSvgFile "t.svg" 
  d <- case f of
    Nothing  -> error "Malformed svg"
    Just svg -> return $ embedDocument svg
  reanimate $ addStatic (mkBackground "cyan") $ staticFrame 1 $ RA.renderDiagram col1

env :: Animation -> Animation
env = mapA $ \svg -> mkGroup
  [ mkBackground "white"
  , withFillOpacity 1 $
    withStrokeWidth 0.1 $
    withStrokeColor "black" (mkGroup [svg]) ]


anim2 :: IO ()
anim2 = do
  let dia :: Diagram D.B
      dia = circle 1 # lwG 0.01 # named "circle"
         <> square 1 # named "square" # lwG 0.01 # (moveTo $ p2 (1, 1))
      dia' = connect "circle" "square" dia
  let tree = RA.renderDiagram dia' 
  reanimate $ addStatic (mkBackground "white") $ staticFrame 1 $ tree

