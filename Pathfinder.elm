module Pathfinder (Obstacle, find) where

type alias Obstacle a =
  { a | position : (Int, Int)
      , size : (Int, Int)
  }

find : List (Obstacle a) -> (Int, Int) -> (Int, Int) -> List (Int, Int)
find obstacles source destination =
  [ ( (fst destination - fst source) // 2
    , (snd destination - snd source) // 2
    )
  , destination
  ]
