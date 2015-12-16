module CategoryView (render) where

import Category exposing (Category)
import Html
import Layers exposing (layers)
import Sprite exposing (Sprite)


categorySprite : Sprite
categorySprite =
  { size = (1, 1)
  , offset = (0, 0)
  , frames = 13
  , src = "img/categories.png"
  }


render : (Float, Float) -> List Html.Attribute -> Category -> Sprite.Box
render position attributes category =
  { sprite  = categorySprite
  , position  = position
  , frame = Category.getFrame category
  , layer = layers.article
  , attributes = attributes
  }
