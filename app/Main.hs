{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import Tree2
import Options.Applicative.Simple
import Brick (simpleMain)
import Disp

main :: IO ()
main = do 
  (opts,()) <- simpleOptions "ver"
                             "header"
                             "desc"
                             (strOption (short 'n') :: Parser String)
                             empty
  simpleMain ui
   
