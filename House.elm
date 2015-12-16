module House (House, house, render) where

import Sprite exposing (Sprite)
import Actions exposing (Action)
import Html.Events exposing (onClick)


type alias House =
  { position : (Int, Int)
  , size : (Int, Int)
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


house : (Int, Int) -> House
house position =
  { position = position
  , size = (3, 2)
  }


render : Signal.Address Action -> House -> List Sprite.Box
render address house =
  [ { sprite = sprite
    , position = house.position
    , layer = 2
    , frame = 0
    , attributes = [onClick address (Actions.GoTo (fst house.position, snd house.position + snd house.size))]
    }
  , { sprite = shadowSprite
    , position = house.position
    , layer = 1
    , frame = 0
    , attributes = []
    }
  ]
