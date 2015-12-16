module Warehouse (Warehouse, warehouse, warehouseSprite, warehouseShadowSprite, warehouseBubbleSprite) where

import Sprite exposing (Sprite)


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
