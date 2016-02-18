module Obstacle (Obstacle, Category(..), animate, fountain, tree) where

import Time exposing (Time)
import AnimationState exposing (animateObject, rotateFrames)
import MapObject exposing (MapObject)


type Category = Fountain | Tree


animate : Time -> Obstacle -> Obstacle
animate time obstacle =
  let
    updateFountain fountain =
      { fountain | frames = rotateFrames fountain.frames }
  in
    case obstacle.category of
      Fountain -> animateObject 150 time updateFountain obstacle
      _ -> obstacle


type alias Obstacle
  = MapObject
      { category : Category
      , elapsed: Time
      , frames : List (Int)
      }


fountain : Obstacle
fountain =
  { category = Fountain
  , position = (0, 0)
  , size = (3, 2)
  , elapsed = 0
  , frames = [0, 1, 2, 3]
  }


tree : Obstacle
tree =
  { category = Tree
  , position = (0, 0)
  , size = (3, 2)
  , elapsed = 0
  , frames = [0]
  }
