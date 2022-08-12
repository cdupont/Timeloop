{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ApplicativeDo #-}
module Main where

import Options.Applicative.Simple
import Brick (simpleMain)
import UI
import TimeLoop.Types

main :: IO ()
main = do 
  ((i,o),()) <- simpleOptions "ver"
                             "header"
                             "desc"
                             options
                             empty
  let (pos1 :: Pos) = readPos i
  let (pos2 :: Pos) = readPos o
  let univ = [Portal pos1 pos2]
  simpleMain $ tableDisplay univ

options :: Parser (String, String)
options = do
  i <- (strOption (short 'i') :: Parser String) 
  o <- (strOption (short 'o') :: Parser String)
  return (i, o)

readPos :: String -> Pos
readPos s = Pos (read x) (read y) (read t) (read d) where
 (x:y:t:d:_) = words s


   
