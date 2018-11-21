module Model where

import Prelude

data Direction
  = North
  | South
  | East
  | West

instance showDirection ∷ Show Direction where
  show North = "↑"
  show South = "↓"
  show East = "→"
  show West = "←"

data State = State
  { x ∷ Int
  , y ∷ Int
  , direction ∷ Direction
  }

instance showState ∷ Show State where
  show (State s) = "{ x: " <> show s.x <> ", y: " <> show s.y <> " direction: " <> (show s.direction) <> " }"
