{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

module Types where


data Dir = U | D | R | L
   deriving (Eq, Ord, Show)

data Pos = Pos {
  x :: Int,
  y :: Int,
  t :: Int}
  deriving (Eq, Ord)

instance Show Pos where
  show (Pos x y t) = show (x, y, t)

