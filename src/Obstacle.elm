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


fountain : (Float, Float) -> Obstacle
fountain position =
  { category = Fountain
  , position = position
  , size = (3, 2)
  , elapsed = 0
  , frames = [0, 1, 2, 3]
  }


tree : (Float, Float) -> Obstacle
tree position =
  { category = Tree
  , position = position
  , size = (3, 2)
  , elapsed = 0
  , frames = [0]
  }
