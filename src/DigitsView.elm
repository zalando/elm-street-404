module DigitsView exposing (render)

import Box exposing (Box)
import Textures
import Layers exposing (layers)


digitsList : Int -> List Int
digitsList n =
    let
        nn =
            n // 10

        r =
            n % 10
    in
        if nn == 0 && r == 0 then
            []
        else
            r :: digitsList nn


render : ( Float, Float ) -> Int -> List Box
render ( x, y ) value =
    let
        digits =
            if value == 0 then
                [ 0 ]
            else
                digitsList value

        renderDigit number digit =
            Box.textured
                Textures.Score
                ( x - toFloat number - 1, y )
                digit
                ( layers.bubble, 0 )
    in
        List.indexedMap renderDigit digits
