module CategoryView (render) where

import Category exposing (Category)
import Html
import Layers exposing (layers)
import Sprite


render : (Float, Float) -> List Html.Attribute -> Category -> List Sprite.Box
render position attributes category =
  Sprite.box
    Sprite.Categories
    position
    (Category.getFrame category)
    (layers.article, if category == Category.Return then 1 else 0)
  ::
  ( if List.length attributes > 0 then
      [ Sprite.empty
          (1, 1)
          (0, 0)
          position
          (layers.clickAbove, 0)
          attributes
      ]
    else
      []
  )
