module PathView (render) where

import Svg exposing (svg, polyline, rect)
import Svg.Attributes exposing (..)
import Html exposing (div, Html)
import Html.Attributes exposing (style)
import Layers exposing (layers)


pointToSring : Int -> (Int, Int) -> String
pointToSring tileSize point =
  (fst point * tileSize + tileSize // 2 |> toString) ++
  "," ++
  (snd point * tileSize + tileSize // 2 |> toString)


renderPoints : Int -> List (Int, Int) -> Html
renderPoints tileSize waypoints =
  polyline
    [ points (List.map (pointToSring tileSize) waypoints
      |> List.foldr (\a b -> a ++ " " ++ b) "")
    , strokeLinejoin "round"
    , strokeLinecap "round"
    , stroke "#bdab82"
    , strokeWidth "40"
    , opacity "0.5"
    , fill "transparent"
    ]
    []


render : (Int, Int) -> Int -> List (Int, Int) -> Html
render (w, h) tileSize route =
  svg
    [ version "1.1"
    , "0 0 " ++ (toString (w * tileSize)) ++
      " " ++ (toString (h * tileSize))
      |> viewBox
    , width (toString (w * tileSize))
    , height (toString (h * tileSize))
    , Html.Attributes.style
      [ "z-index" => toString layers.route
      , "position" => "absolute"
      ]
    ]
    [ renderPoints tileSize route]


(=>) : a -> b -> (a, b)
(=>) = (,)
