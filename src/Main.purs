module Main where

import Prelude (Unit, bind, discard, show, ($), (>>=))
import Effect (Effect)
import Effect.Console (log)
import FreeDsl (Dsl, forward, left, right, backward, interpretDsl)
import Model (Direction(..), State(..), Coord(..))

program ∷ State → Dsl State
program init = 
  forward init
  >>= left
  >>= forward
  >>= right
  >>= forward
  >>= left
  >>= forward
  >>= backward

initial ∷ State 
initial = State { x: Ten, y: One, direction: East }

main ∷ Effect Unit
main = do
  log "Starting..."
  log $ show initial
  s ← interpretDsl $ program initial
  log "Done!"
