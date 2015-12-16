module House (House, house, render) where

import Sprite exposing (Sprite)
import Actions exposing (Action)
import Html.Events exposing (onClick)
import Layers exposing (layers)


type alias House =
  { position : (Float, Float)
  , size : (Float, Float)
  }


sprite : Sprite
sprite =
  { size = (2, 3)
  , offset = (0, -1)
  , frames = 1
  , src = "img/house.png"
  }


shadowSprite : Sprite
shadowSprite =
  { size = (3, 2)
  , offset = (0, 1)
  , frames = 1
  , src = "img/house-shadow.png"
  }


bubbleSprite1 : Sprite
bubbleSprite1 =
  { size = (3, 3)
  , offset = (-2, -1)
  , frames = 1
  , src = "img/house-bubble-1.png"
  }


bubbleSprite2 : Sprite
bubbleSprite2 =
  { size = (3, 4)
  , offset = (-2, -2)
  , frames = 1
  , src = "img/house-bubble-2.png"
  }


bubbleSprite3 : Sprite
bubbleSprite3 =
  { size = (3, 5)
  , offset = (-2, -3)
  , frames = 1
  , src = "img/house-bubble-3.png"
  }


house : (Float, Float) -> House
house position =
  { position = position
  , size = (3, 2)
  }


render : Signal.Address Action -> House -> List Sprite.Box
render address house =
  [ { sprite = sprite
    , position = house.position
    , layer = layers.obstacle
    , frame = 0
    , attributes = [onClick address (Actions.GoTo (round (fst house.position), round (snd house.position + snd house.size)))]
    }
  , { sprite = shadowSprite
    , position = house.position
    , layer = layers.shadow
    , frame = 0
    , attributes = []
    }
  ]
