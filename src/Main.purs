module Main where

import Prelude (Unit, bind, discard, show, ($), (>>=))
import Effect (Effect)
import Effect.Console (log)
import FreeDsl (Dsl, forward, left, interpretDsl)
import Model (Direction(..), State(..))

program :: State -> Dsl State
program init = 
  forward init
  >>= left
  >>= left
  >>= forward

initial :: State 
initial = State { x: 0, y: 0, direction: East }

main :: Effect Unit
main = do
  log "Starting..."
  log $ show initial
  s <- interpretDsl $ program initial
  log "Done!"
