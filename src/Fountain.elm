module Fountain exposing (Fountain, animate, fountain)

import Time exposing (Time)
import AnimationState exposing (AnimatedObject, animateFrame)


animate : Time -> Fountain -> Fountain
animate =
    animateFrame 3


type alias Fountain =
    AnimatedObject {}


fountain : Fountain
fountain =
    { elapsed = 0
    , timeout = 150
    , frame = 0
    }
