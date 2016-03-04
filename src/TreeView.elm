module TreeView (render) where

import Sprite
import Layers exposing (layers)
import MapObject exposing (MapObject)


render : MapObject -> List Sprite.Box
render {position} =
  [ Sprite.box Sprite.Tree position 0 (layers.obstacle, 0) ]
