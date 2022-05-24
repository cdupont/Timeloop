{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import Diag
import Anim
import Tree
import Options.Applicative.Simple

main :: IO ()
main = do 
  (opts,()) <- simpleOptions "ver"
                             "header"
                             "desc"
                             (flag () () (long "some-flag"))
                             empty
  let paths = goodTravs start smallUCol
  showGoodTravs paths
   
