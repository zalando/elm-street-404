module Fountain (Fountain, animate, fountain) where

import Time exposing (Time)
import AnimationState exposing (AnimatedObject, animateObject, rotateFrames)


animate : Time -> Fountain -> Fountain
animate time =
  animateObject time rotateFrames


type alias Fountain =
  AnimatedObject {frames : List Int}


fountain : Fountain
fountain =
  { elapsed = 0
  , timeout = 150
  , frames = [0..3]
  }
