module FountainView (render) where

import Sprite
import Layers exposing (layers)
import MapObject exposing (MapObject, MapObjectCategory(..))
import Fountain exposing (Fountain)


fountainShadowSprite : Sprite.Sprite
fountainShadowSprite =
  { size = (4, 2)
  , offset = (0, 1)
  , frames = 1
  , src = "fountain-shadow.png"
  }


fountainSprite : Sprite.Sprite
fountainSprite =
  { size = (3, 2)
  , offset = (0, 0)
  , frames = 1
  , src = "fountain.png"
  }


fountainSpringSprite : Sprite.Sprite
fountainSpringSprite =
  { size = (1, 2)
  , offset = (1, -1)
  , frames = 4
  , src = "fountain-spring.png"
  }


render : Fountain -> MapObject -> List Sprite.Box
render fountain obstacle =
  [ { sprite = fountainSprite
    , position = obstacle.position
    , layer = layers.obstacle
    , frame = 0
    , attributes = []
    }
  , { sprite = fountainSpringSprite
    , position = obstacle.position
    , layer = layers.fountainSpring
    , frame = Maybe.withDefault 0 (List.head fountain.frames)
    , attributes = []
    }
  , { sprite = fountainShadowSprite
    , position = obstacle.position
    , layer = layers.shadow
    , frame = 0
    , attributes = []
    }
  ]
