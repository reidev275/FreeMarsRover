module FreeDsl where

import Prelude
import Control.Monad.Free (Free, liftF, foldFree)
import Effect (Effect)
import Effect.Console (log)

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

data DslF a
  = Forward State (State -> a)
  | Left State (State -> a)

type Dsl = Free DslF

forward :: State -> Dsl State
forward state = liftF $ Forward state \a -> a

left :: State -> Dsl State
left state = liftF $ Left state \a -> a


leftImpl :: State -> State
leftImpl (State s) =
  case s.direction of
    North -> State s { direction = West }
    South -> State s { direction = East }
    East  -> State s { direction = North }
    West  -> State s { direction = South }

forwardImpl :: State -> State
forwardImpl (State s) = 
  case s.direction of
    North -> State s { x = s.x + 1 }
    South -> State s { x = s.x - 1 }
    East  -> State s { y = s.y + 1 }
    West  -> State s { y = s.y - 1 }


interpretDsl :: Dsl ~> Effect
interpretDsl = foldFree morph
  where
    morph :: DslF ~> Effect
    morph (Forward state next) = do
      log $ show s1
      pure $ next s1
      where
        s1 = forwardImpl state
    morph (Left state next) = do
      log $ show s1
      pure $ next s1
      where
        s1 = leftImpl state

