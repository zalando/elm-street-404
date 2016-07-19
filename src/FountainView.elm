module FountainView exposing (render)

import Box exposing (Box)
import Textures
import Layers exposing (layers)
import MapObject exposing (MapObject)
import Fountain exposing (Fountain)


render : Fountain -> MapObject -> List Box
render {frame} {position} =
  [ Box.textured Textures.Fountain position 0 (layers.obstacle, 0)
  , Box.offsetTextured (1, -1) Textures.FountainSpring position frame (layers.obstacle, 1)
  , Box.offsetTextured (0, 1) Textures.FountainShadow position 0 (layers.shadow, 0)
  ]
