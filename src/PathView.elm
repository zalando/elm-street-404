module PathView (render) where

import Svg exposing (svg, polyline, rect)
import Svg.Attributes exposing (..)
import Html exposing (div, Html)


addPointToSring : Int -> (Int, Int) -> String -> String
addPointToSring tileSize (x, y) =
  (++)
    ( toString (x * tileSize + tileSize) ++
      "," ++
      toString (y * tileSize + tileSize // 2) ++
      " "
    )


renderPoints : Int -> List (Int, Int) -> Html
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


render : (Int, Int) -> Int -> List (Int, Int) -> Html
render (w, h) tileSize route =
  svg
    [ version "1.1"
    , viewBox ("0 0 " ++ toString (w * tileSize) ++ " " ++ toString (h * tileSize))
    , width (toString (w * tileSize))
    , height (toString (h * tileSize))
    , style ("position: absolute;")
    ]
    [ renderPoints tileSize route ]


(=>) : a -> b -> (a, b)
(=>) = (,)
