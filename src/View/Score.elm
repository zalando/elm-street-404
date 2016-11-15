module View.Score exposing (render)

import Box exposing (Box)
import Textures
import Layers exposing (layers)
import View.Digits


render : ( Int, Int ) -> Int -> Int -> Int -> List Box
render ( width, _ ) score maxLives lives =
    let
        x =
            toFloat width - 2

        renderLife number =
            Box.textured
                Textures.Score
                ( toFloat (maxLives - number), 1 )
                (if number >= maxLives - lives then
                    11
                 else
                    12
                )
                ( layers.bubble, 0 )
    in
        Box.textured Textures.Score ( x, 1 ) 10 ( layers.bubble, 0 )
            :: View.Digits.render ( x, 1 ) (score * 10)
            ++ List.map renderLife (List.range 0 (maxLives - 1))
