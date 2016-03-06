module CategoryView (render) where

import Category exposing (Category)
import Layers exposing (layers)
import Box exposing (Box)
import Textures
import Actions exposing (Action)


render : (Float, Float) -> Maybe Action -> Category -> List Box
render position maybeOnClick category =
  Box.textured
    Textures.Categories
    position
    (Category.getFrame category)
    (layers.article, if category == Category.Return then 1 else 0)
  ::
  ( case maybeOnClick of
      Nothing ->
        []
      Just onClick ->
        [ Box.clickable (1, 1) (0, 0) position (layers.clickAbove, 0) onClick ]
  )
