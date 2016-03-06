module FountainView (render) where

import Box exposing (Box)
import Textures
import Layers exposing (layers)
import MapObject exposing (MapObject)
import Fountain exposing (Fountain)


render : Fountain -> MapObject -> List Box
render {frames} {position} =
  let
    frame = (Maybe.withDefault 0 (List.head frames))
  in
    [ Box.textured Textures.Fountain position 0 (layers.obstacle, 0)
    , Box.textured Textures.FountainSpring position frame (layers.obstacle, 1)
    , Box.textured Textures.FountainShadow position 0 (layers.shadow, 0)
    ]
