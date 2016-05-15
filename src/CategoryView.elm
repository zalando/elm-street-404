module CategoryView exposing (render)

import Category exposing (Category)
import Layers exposing (layers)
import Box exposing (Box)
import Textures


render : (Float, Float) -> Category -> Box
render position category =
  Box.textured
    Textures.Categories
    position
    (Category.getFrame category)
    (layers.article, if category == Category.Return then 1 else 0)
