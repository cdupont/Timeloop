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
  let (pos :: Pos) = readPos i
  putStrLn $ show pos

options :: Parser (String, String)
options = do
  i <- (strOption (short 'i') :: Parser String) 
  o <- (strOption (short 'o') :: Parser String)
  return (i, o)


readPos :: String -> Pos
readPos (x:' ':y:' ':t:' ':d:_) = Pos (read [x]) (read [y]) (read [t]) (read [d])


  --simpleMain tableDisplay 
   
