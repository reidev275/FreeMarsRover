module Model where

import Prelude

data Direction
  = North
  | South
  | East
  | West

instance showDirection :: Show Direction where
  show North = "North"
  show South = "South"
  show East = "East"
  show West = "West"

data State = State
  { x :: Int
  , y :: Int
  , direction :: Direction
  }

instance showState :: Show State where
  show (State s) = "At " <> show s.x <> ", " <> show s.y <> " facing " <> (show s.direction)
