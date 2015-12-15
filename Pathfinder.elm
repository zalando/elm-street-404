module Pathfinder (Obstacle, find, render) where
import Svg exposing (svg, polyline)
import Svg.Attributes exposing (..)
import Html exposing (Html)
import Astar exposing (astar)

type alias Obstacle a =
  { a | position : (Int, Int)
      , size : (Int, Int)
  }

find : (Int, Int) -> List (Obstacle a) -> (Int, Int) -> (Int, Int) -> List (Int, Int)
find gridSize obstacles start destination =
  astar gridSize (List.map (\ o -> o.size) obstacles) start destination
  -- [ ( (fst destination - fst source) // 2 + fst source
  --   , (snd destination - snd source) // 2 + snd source
  --   )
  -- , destination
  -- ]


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
