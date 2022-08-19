{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ApplicativeDo #-}
module Main where

import Options.Applicative.Simple
import Brick (simpleMain, defaultMain, hBox)
import UI
import TimeLoop.Types
import TimeLoop.Search


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

  let initState = UI portal1 1 Entry 
  _ <- defaultMain app initState

  putStrLn "Goodbye"

  --simpleMain $ tableDisplay univ
  --putStrLn $ show $ take 2 $ search1




options :: Parser (String, String)
options = do
  i <- (strOption (short 'i') :: Parser String) 
  o <- (strOption (short 'o') :: Parser String)
  return (i, o)

readPos :: String -> PTD
readPos s = PTD (Pos (read x) (read y)) (read t) (read d) where
 (x:y:t:d:_) = words s


   
