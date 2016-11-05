module PlaceholderView exposing (render)

import Layers exposing (layers)
import Box exposing (Box)
import Textures


render : ( Float, Float ) -> Box
render position =
    Box.textured
        Textures.Categories
        position
        12
        ( layers.article
        , 0
        )
