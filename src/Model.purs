module Model where

import Prelude (class Show, show, (<>))

data Coord
  = One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten

instance showCoord ∷ Show Coord where
  show One   = "1"
  show Two   = "2"
  show Three = "3"
  show Four  = "4"
  show Five  = "5"
  show Six   = "6"
  show Seven = "7"
  show Eight = "8"
  show Nine  = "9"
  show Ten   = "10"

succ ∷ Coord → Coord
succ One   = Two
succ Two   = Three
succ Three = Four
succ Four  = Five
succ Five  = Six
succ Six   = Seven
succ Seven = Eight
succ Eight = Nine
succ Nine  = Ten
succ Ten   = One

pred ∷ Coord → Coord
pred One   = Ten
pred Two   = One
pred Three = Two
pred Four  = Three
pred Five  = Four
pred Six   = Five
pred Seven = Six
pred Eight = Seven
pred Nine  = Eight
pred Ten   = Nine

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
  { x ∷ Coord
  , y ∷ Coord
  , direction ∷ Direction
  }

instance showState ∷ Show State where
  show (State s) = "{ x: " <> show s.x <> ", y: " <> show s.y <> " direction: " <> (show s.direction) <> " }"
