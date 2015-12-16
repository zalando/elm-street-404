module Warehouse (Warehouse, warehouse, render) where

import Sprite exposing (Sprite)
import Actions exposing (Action)
import Html.Events exposing (onClick)
import Layers exposing (layers)


type alias Warehouse =
  { position : (Float, Float)
  , size : (Float, Float)
  }


warehouseSprite : Sprite
warehouseSprite =
  { size = (4, 4)
  , offset = (0, -1)
  , frames = 1
  , src = "img/warehouse.png"
  }


warehouseShadowSprite : Sprite
warehouseShadowSprite =
  { size = (5, 4)
  , offset = (0, 0)
  , frames = 1
  , src = "img/warehouse-shadow.png"
  }


warehouseBubbleSprite : Sprite
warehouseBubbleSprite =
  { size = (4, 5)
  , offset = (-2, -3)
  , frames = 1
  , src = "img/warehouse-bubble.png"
  }


warehouse : (Float, Float) -> Warehouse
warehouse position =
  { position = position
  , size = (3, 3)
  }


render : Signal.Address Action -> Warehouse -> List Sprite.Box
render address warehouse =
  [ { sprite = warehouseSprite
    , position = warehouse.position
    , layer = layers.obstacle
    , frame = 0
    , attributes = [onClick address (Actions.GoTo (round (fst warehouse.position) + 1, round (snd warehouse.position + snd warehouse.size)))]
    }
  , { sprite = warehouseShadowSprite
    , position = warehouse.position
    , layer = layers.shadow
    , frame = 0
    , attributes = []
    }
  , { sprite = warehouseBubbleSprite
    , position = warehouse.position
    , layer = layers.bubble
    , frame = 0
    , attributes = []
    }
  ]
