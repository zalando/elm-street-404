module HouseView (render) where

import Actions exposing (Action)
import Html.Events exposing (onClick)
import Layers exposing (layers)
import Sprite exposing (Sprite)
import House exposing (House)
import Request exposing (Request)


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


render : Signal.Address Action -> List Request -> House -> List Sprite.Box
render address requests house =
  [ { sprite = sprite
    , position = house.position
    , layer = layers.obstacle
    , frame = 0
    , attributes =
      [ onClick address (Actions.GoTo (round (fst house.position), round (snd house.position + snd house.size))) ]
    }
  , { sprite = shadowSprite
    , position = house.position
    , layer = layers.shadow
    , frame = 0
    , attributes = []
    }
  ]
