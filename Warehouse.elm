module Warehouse (Warehouse, warehouse, render) where

import Sprite exposing (Sprite)
import Actions exposing (Action)
import Html.Events exposing (onClick)


type alias Warehouse =
  { position : (Int, Int)
  , size : (Int, Int)
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


warehouse : (Int, Int) -> Warehouse
warehouse position =
  { position = position
  , size = (3, 3)
  }


render : Signal.Address Action -> Warehouse -> List Sprite.Box
render address warehouse =
  [ { sprite = warehouseSprite
    , position = warehouse.position
    , layer = 2
    , frame = 0
    , attributes = [onClick address (Actions.GoTo (fst warehouse.position + 1, snd warehouse.position + snd warehouse.size))]
    }
  , { sprite = warehouseShadowSprite
    , position = warehouse.position
    , layer = 1
    , frame = 0
    , attributes = []
    }
  , { sprite = warehouseBubbleSprite
    , position = warehouse.position
    , layer = 3
    , frame = 0
    , attributes = []
    }
  ]
