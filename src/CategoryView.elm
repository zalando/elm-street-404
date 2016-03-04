module CategoryView (render) where

import Category exposing (Category)
import Html
import Layers exposing (layers)
import Sprite


render : (Float, Float) -> Maybe Html.Attribute -> Category -> List Sprite.Box
render position maybeOnClick category =
  Sprite.box
    Sprite.Categories
    position
    (Category.getFrame category)
    (layers.article, if category == Category.Return then 1 else 0)
  ::
  ( case maybeOnClick of
      Nothing ->
        []
      Just onClick ->
        [ Sprite.clickable
            (1, 1)
            (0, 0)
            position
            (layers.clickAbove, 0)
            onClick
        ]
  )
