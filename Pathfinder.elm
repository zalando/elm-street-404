module Pathfinder (Obstacle, find, render, main) where
import Svg exposing (svg, polyline, rect)
import Svg.Attributes exposing (..)
import Html exposing (Html)
import Html exposing (div, Html)
import Html.Attributes exposing (style)
import Mouse
import Debug

type alias Obstacle a =
  { a | position : (Int, Int)
      , size : (Int, Int)
  }

find : (Int, Int) -> List (Obstacle a) -> (Int, Int) -> (Int, Int) -> List (Int, Int)
find gridSize obstacles source destination =
  Debug.watch
    "route"
    [ ( (fst destination - fst source) // 4 + fst source
      , (snd destination - snd source) // 4 + snd source
      )
    , ( (fst destination - fst source) // 2 + fst source
      , (snd destination - snd source) // 2 + snd source
      )
    , ( (fst destination - fst source) // 4 * 3 + fst source
      , (snd destination - snd source) // 4 * 3 + snd source
      )
    , destination
    ]


pointToSring : Int -> (Int, Int) -> String
pointToSring tileSize point =
  toString (fst point * tileSize) ++ "," ++ toString (snd point * tileSize)


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


renderObstacle : Int -> Obstacle a -> Html
renderObstacle tileSize obstacle =
  rect
    [ x (toString (fst obstacle.position * tileSize))
    , y (toString (snd obstacle.position * tileSize))
    , width (toString (fst obstacle.size * tileSize))
    , height (toString (snd obstacle.size * tileSize))
    , stroke "#9b8960"
    , strokeWidth "2"
    , fill "transparent"
    ]
    []


render : Int -> List (Obstacle a) -> List (Int, Int) -> (Int, Int) -> Html
render tileSize obstacles points source =
  svg
    [version "1.1", viewBox "0 0 960 560"]
    (renderPoints tileSize (source :: points) :: List.map (renderObstacle tileSize) obstacles)


(=>) : a -> b -> (a, b)
(=>) = (,)


main : Signal Html
main =
  Signal.map renderMain (Signal.sampleOn Mouse.clicks Mouse.position)


renderMain : (Int, Int) -> Html
renderMain click =
  let
    tileSize = 40
    dest = (fst click // tileSize, snd click // tileSize)
    obstacles =
      [ {position = (3, 5), size = (2, 2)}
      , {position = (15, 8), size = (2, 2)}
      ]
    source = (5, 5)
  in
    div
    [ Html.Attributes.style
      [ "height" => "560px"
      , "position" => "relative"
      , "width" => "960px"
      , "background-image" => "url(img/bg-grid.jpg)"
      , "background-size" => "960px 560px"
      ]
    ]
    [ render tileSize obstacles (find source obstacles source dest) source ]
