module FreeDsl where

import Prelude (type (~>), discard, pure, show, ($), (+), (-), identity)
import Control.Monad.Free (Free, liftF, foldFree)
import Effect (Effect)
import Effect.Console (log)
import Model (Direction(..), State(..))

data DslF a
  = Forward State (State → a)
  | Backward State (State → a)
  | Left State (State → a)
  | Right State (State → a)

type Dsl = Free DslF

forward ∷ State → Dsl State
forward state = liftF $ Forward state identity

backward ∷ State → Dsl State
backward state = liftF $ Backward state identity

left ∷ State → Dsl State
left state = liftF $ Left state identity

right ∷ State → Dsl State
right state = liftF $ Right state identity

leftImpl ∷ State → State
leftImpl (State s) =
  case s.direction of
    North → State s { direction = West }
    South → State s { direction = East }
    East  → State s { direction = North }
    West  → State s { direction = South }

rightImpl ∷ State → State
rightImpl (State s) = 
  case s.direction of
    North → State s { direction = East }
    South → State s { direction = West }
    East  → State s { direction = South }
    West  → State s { direction = North }

forwardImpl ∷ State → State
forwardImpl (State s) = 
  case s.direction of
    North → State s { x = s.x + 1 }
    South → State s { x = s.x - 1 }
    East  → State s { y = s.y + 1 }
    West  → State s { y = s.y - 1 }


backwardImpl ∷ State → State
backwardImpl (State s) = 
  case s.direction of
    North → State s { x = s.x - 1 }
    South → State s { x = s.x + 1 }
    East  → State s { y = s.y - 1 }
    West  → State s { y = s.y + 1 }

display ∷ ∀ a. (State → State) → State → (State → a) → Effect a
display impl state next = do
  log $ show s1
  pure $ next s1
  where
    s1 = impl state

interpretDsl ∷ Dsl ~> Effect
interpretDsl = foldFree morph
  where
    morph :: DslF ~> Effect
    morph (Forward state next) =
      display forwardImpl state next
    morph (Left state next) = 
      display leftImpl state next
    morph (Backward state next) =
      display backwardImpl state next
    morph (Right state next) = do
      display rightImpl state next

