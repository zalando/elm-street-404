module Pathfinder (Obstacle, find, render) where
import Svg exposing (svg, polyline)
import Svg.Attributes exposing (..)
import Html exposing (Html)

type alias Obstacle a =
  { a | position : (Int, Int)
      , size : (Int, Int)
  }

find : List (Obstacle a) -> (Int, Int) -> (Int, Int) -> List (Int, Int)
find obstacles source destination =
  [ ( (fst destination - fst source) // 2 + fst source
    , (snd destination - snd source) // 2 + snd source
    )
  , destination
  ]

connectPoints : Int -> (Int, Int) -> (Int, Int) -> Html
connectPoints tileSize p1 p2 =
  Svg.line
  [
    x1 (toString (fst p1 * tileSize)),
    y1 (toString (snd p1 * tileSize)),
    x2 (toString (fst p2 * tileSize)),
    y2 (toString (snd p2 * tileSize)),
    strokeLinejoin "round",
    strokeLinecap "round",
    stroke "#bdab82",
    strokeWidth "40",
    opacity "0.5"
  ]
  []


pointToSring : Int -> (Int, Int) -> String
pointToSring tileSize point =
  toString (fst point * tileSize) ++ "," ++ toString (snd point * tileSize)


renderPoints : Int -> List (Int, Int) -> Html
renderPoints tileSize waypoints =
  polyline
    [ points (List.map (pointToSring tileSize) waypoints |> List.foldr (\a b -> a ++ " " ++ b) ""),
      strokeLinejoin "round",
      strokeLinecap "round",
      stroke "#bdab82",
      strokeWidth "40",
      opacity "0.5"
    ]
    []


render : Int -> List (Int, Int) -> (Int, Int) -> Html
render tileSize points source =
  svg
    [version "1.1", viewBox "0 0 960 560"]
    [renderPoints tileSize (source :: points)]
