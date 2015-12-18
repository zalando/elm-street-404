module Generator (Generator, animate, initial) where

import AnimationState
import Time exposing (Time)


type alias Generator =
  { elapsed: Time
  , timeout : Time
  , active : Bool
  }


initial : Time -> Generator
initial timeout =
  Generator 0 timeout False


animate : Time -> Generator -> Generator
animate time generator =
  let
    updateGenerator generator =
      { generator | active = True }
  in
    AnimationState.animateObject
      generator.timeout
      time
      updateGenerator
      { generator | active = False }
