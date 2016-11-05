module PathView exposing (render)

import Svg exposing (svg, polyline)
import Svg.Attributes exposing (..)
import Html exposing (Html)
import Actions exposing (Action)


addPointToSring : Int -> ( Int, Int ) -> String -> String
addPointToSring tileSize ( x, y ) =
    (++)
        (" "
            ++ toString (x * tileSize + tileSize)
            ++ ","
            ++ toString (y * tileSize + tileSize // 2)
        )


renderPoints : Int -> List ( Int, Int ) -> Html Action
renderPoints tileSize waypoints =
    polyline
        [ points (List.foldl (addPointToSring tileSize) "" waypoints)
        , strokeLinejoin "round"
        , strokeLinecap "round"
        , stroke "#bdab82"
        , strokeWidth (toString tileSize)
        , opacity "0.5"
        , fill "transparent"
        ]
        []


render : ( Int, Int ) -> Int -> List ( Int, Int ) -> Html Action
render ( w, h ) tileSize route =
    svg
        [ version "1.1"
        , viewBox ("0 0 " ++ toString (w * tileSize) ++ " " ++ toString (h * tileSize))
        , width (toString (w * tileSize))
        , height (toString (h * tileSize))
        , style "position: absolute"
        ]
        [ renderPoints tileSize route ]
