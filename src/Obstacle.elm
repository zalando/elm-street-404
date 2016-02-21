module Obstacle (Obstacle, Category(..), animate, fountain, tree) where

import Time exposing (Time)
import AnimationState exposing (AnimatedObject, animateObject, rotateFrames)
import MapObject exposing (MapObject)


type Category = Fountain | Tree


animate : Time -> Obstacle -> Obstacle
animate time obstacle =
  case obstacle.category of
    Fountain -> animateObject time rotateFrames obstacle
    _ -> obstacle


type alias Obstacle =
  AnimatedObject
    ( MapObject
        { category : Category
        , frames : List (Int)
        }
    )


fountain : Obstacle
fountain =
  { category = Fountain
  , position = (0, 0)
  , size = (3, 2)
  , elapsed = 0
  , timeout = 150
  , frames = [0, 1, 2, 3]
  }


tree : Obstacle
tree =
  { category = Tree
  , position = (0, 0)
  , size = (3, 2)
  , elapsed = 0
  , timeout = 0
  , frames = [0]
  }
