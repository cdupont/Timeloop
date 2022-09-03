{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ApplicativeDo #-}
module Main where

import Options.Applicative.Simple
import Brick (customMain, hBox)
import UI
import TimeLoop.Types
import TimeLoop.Search
import Brick.BChan
import qualified Graphics.Vty as V
import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (void, forever)

main :: IO ()
main = do 
  --((i,o),()) <- simpleOptions "ver"
  --                           "header"
  --                           "desc"
  --                           options
  --                           empty
  --let (pos1 :: PTD) = readPos i
  --let (pos2 :: PTD) = readPos o
  --let univ = [Portal pos1 pos2]
  chan <- newBChan 10
  void . forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 1000000
  putStrLn "Loading"
  let initState = UI univ1 (SelItem EntryPortal 1) 0
  let buildVty = V.mkVty V.defaultConfig
  initialVty <- buildVty
  customMain initialVty buildVty (Just chan) app initState

  putStrLn "Goodbye"




options :: Parser (String, String)
options = do
  i <- (strOption (short 'i') :: Parser String) 
  o <- (strOption (short 'o') :: Parser String)
  return (i, o)

readPos :: String -> PTD
readPos s = PTD (Pos (read x) (read y)) (read t) (read d) where
 (x:y:t:d:_) = words s


   
