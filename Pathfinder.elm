module Pathfinder (Obstacle, obstacleTiles, find, render, main) where

import Svg exposing (svg, polyline, rect)
import Svg.Attributes exposing (..)
import Html exposing (Html)
import Astar exposing (astar)
import Html exposing (div, Html)
import Html.Attributes exposing (style)
import Mouse
import Layers exposing (layers)

type alias Obstacle a =
  { a | position : (Float, Float)
      , size : (Float, Float)
  }

obstacleRow : (Int, Int) -> Int -> Int -> List (Int, Int)
obstacleRow position rowIndex columns =
  case columns of
    0 -> []
    _ -> (fst position + rowIndex, snd position + columns - 1) ::
      obstacleRow position rowIndex (columns - 1)

obstacleToTiles : (Int, Int) -> (Int, Int) -> List (Int, Int)
obstacleToTiles position size =
  case fst size of
    0 -> []
    _ -> obstacleRow position (fst size - 1) (snd size) ++
      obstacleToTiles position (fst size - 1, snd size)


toIntTuple : (Float, Float) -> (Int, Int)
toIntTuple (a, b) = (round a, round b)

obstacleTiles : List (Obstacle a) -> List (Int, Int)
obstacleTiles obstacles =
  List.concat (List.map (\ {position, size} -> obstacleToTiles (toIntTuple position) (toIntTuple size)) obstacles)

find : (Int, Int) -> List (Int, Int) -> (Int, Int) -> (Int, Int) -> List (Int, Int)
find = astar


pointToSring : Int -> (Int, Int) -> String
pointToSring tileSize point =
  toString (fst point * tileSize + tileSize // 2) ++ "," ++ toString (snd point * tileSize + tileSize // 2)


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


renderObstacleTest : Int -> (Int, Int) -> Html
renderObstacleTest tileSize position =
  rect
    [ x (toString (fst position * tileSize))
    , y (toString (snd position * tileSize))
    , width (toString tileSize)
    , height (toString tileSize)
    , stroke "#9b8960"
    , strokeWidth "2"
    , fill "transparent"
    ]
    []


render' : Int -> List (Obstacle a) -> List (Int, Int) -> (Int, Int) -> Html
render' tileSize obstacles points source =
  svg
    [ version "1.1"
    , viewBox "0 0 960 560"
    ]
    ( renderPoints tileSize (source :: points) ::
      List.map (renderObstacleTest tileSize) (obstacleTiles obstacles)
    )


render : (Int, Int) -> Int -> List (Int, Int) -> Html
render (w, h) tileSize route =
  svg
    [ version "1.1"
    , viewBox ("0 0 " ++ (toString (w * tileSize)) ++ " " ++ (toString (h * tileSize)))
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


main : Signal Html
main =
  Signal.map renderMain (Signal.sampleOn Mouse.clicks Mouse.position)


renderMain : (Int, Int) -> Html
renderMain click =
  let
    tileSize = 40
    dest = (fst click // tileSize, snd click // tileSize)
    obstacles =
      [ {position = (3, 5), size = (3, 3)}
      , {position = (9, 6), size = (3, 3)}
      , {position = (15, 6), size = (3, 3)}
      , {position = (5, 2), size = (1, 3)}
      , {position = (3, 1), size = (3, 1)}
      ]
    source = (1, 1)
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
    [ render' tileSize obstacles (find (36, 36) (obstacleTiles obstacles) source dest) source ]
