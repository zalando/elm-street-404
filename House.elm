module House (House, house, render) where
import Sprite exposing (Sprite)

type alias House =
  { position : (Int, Int)
  , size : (Int, Int)
  }


houseSprite : Sprite
houseSprite =
  { size = (2, 3)
  , offset = (0, -1)
  , frames = 1
  , src = "img/house.png"
  }


houseShadowSprite : Sprite
houseShadowSprite =
  { size = (3, 2)
  , offset = (0, 1)
  , frames = 1
  , src = "img/house-shadow.png"
  }


house : (Int, Int) -> House
house position =
  { position = position
  , size = (3, 2)
  }


render : House -> List Sprite.Box
render house =
  [ { sprite = houseSprite
    , position = house.position
    , layer = 2
    , frame = 0
    , attributes = []
    }
  , { sprite = houseShadowSprite
    , position = house.position
    , layer = 1
    , frame = 0
    , attributes = []
    }
  ]
