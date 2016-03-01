module FountainView (render) where

import Sprite
import Layers exposing (layers)
import MapObject exposing (MapObject)
import Fountain exposing (Fountain)


render : Fountain -> MapObject -> List Sprite.Box
render {frames} {position} =
  [ Sprite.box
      Sprite.Fountain
      position
      0
      (layers.obstacle, 0)
  , Sprite.box
      Sprite.FountainSpring
      position
      (Maybe.withDefault 0 (List.head frames))
      (layers.obstacle, 1)
  , Sprite.box
      Sprite.FountainShadow
      position
      0
      (layers.shadow, 0)
  ]
