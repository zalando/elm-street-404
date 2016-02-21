module TreeView (render) where

import Sprite
import Layers exposing (layers)
import MapObject exposing (MapObject)


treeSprite : Sprite.Sprite
treeSprite =
  { size = (3, 5)
  , offset = (0, -3)
  , frames = 1
  , src = "tree.png"
  }


render : MapObject -> List Sprite.Box
render obstacle =
  [ { sprite = treeSprite
    , position = obstacle.position
    , layer = layers.obstacle
    , frame = 0
    , attributes = []
    }
  ]
