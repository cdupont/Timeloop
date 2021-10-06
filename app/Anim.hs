{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}

module Anim where


import           Reanimate
import           Reanimate.Builtin.Documentation

anim :: IO ()
--anim = reanimate $ docEnv $ playThenReverseA drawCircle
anim = reanimate $ animate $ const ((mkCircle 1))

env :: Animation -> Animation
env = mapA $ \svg -> mkGroup
  [ mkBackground "white"
  , withFillOpacity 1 $
    withStrokeWidth 0.1 $
    withStrokeColor "black" (mkGroup [svg]) ]

a = reanimate $ env $ animate $ const $ gridLayout $ replicate 10 $ replicate 10 $ mkCircle 0.1

arrow = mkGroup [mkLinePathClosed [(0.9, 0.1), (1, 0), (0.9, -0.1)],
                 mkLine (0, 0) (0.9, 0)]

