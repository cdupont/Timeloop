{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Options.Applicative.Simple
import Brick (customMain, hBox)
import UI
import TimeLoop.Types
import TimeLoop.Search
import Brick.BChan
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform as VCP
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
    threadDelay 100000
  putStrLn "Loading"
  let initState = UI univ2 (Just (SelItem EntryPortal 0)) 0 (Config False False)
  let buildVty = VCP.mkVty V.defaultConfig
  initialVty <- buildVty
  a <- customMain initialVty buildVty (Just chan) app initState
  putStrLn $ show a.initUniv
  putStrLn "Goodbye"


options :: Parser (String, String)
options = do
  i <- (strOption (short 'i') :: Parser String) 
  o <- (strOption (short 'o') :: Parser String)
  return (i, o)


