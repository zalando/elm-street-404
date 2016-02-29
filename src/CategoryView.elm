module CategoryView (render) where

import Category exposing (Category)
import Html
import Layers exposing (layers)
import Sprite exposing (Sprite)


categorySprite : Sprite
categorySprite =
  { size = (1, 1)
  , offset = (0, 0)
  , frames = 14
  , src = "categories.png"
  }


render : (Float, Float) -> List Html.Attribute -> Category -> List Sprite.Box
render position attributes category =
  [ { sprite = categorySprite
    , position = position
    , frame = Category.getFrame category
    , layer = (layers.article, if category == Category.Return then 1 else 0)
    , attributes = attributes
    }
  ]
