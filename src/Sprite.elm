module Sprite (Sprite, Box, empty, sort, render) where

import Html.Attributes exposing (style)
import Html exposing (div)


type alias Sprite =
  { size : (Int, Int)
  , offset : (Float, Float)
  , frames : Int
  , src : String
  }


type alias Box =
  { position : (Float, Float)
  , sprite : Sprite
  , frame : Int
  , layer : Int
  , attributes : List Html.Attribute
  }


empty : (Int, Int) -> (Float, Float) -> Sprite
empty size offset =
  { size = size
  , offset = offset
  , frames = 0
  , src = ""
  }


(=>) : a -> b -> (a, b)
(=>) = (,)


sort : List Box -> List Box
sort boxes =
  List.sortBy (\box -> snd box.position) boxes


render : String -> Int -> Box -> Html.Html
render imagesUrl tileSize ({sprite, position, frame, layer, attributes} as box) =
  let
    (width, height) = sprite.size
    left = round ((fst position + fst sprite.offset) * toFloat tileSize)
    top = round ((snd position + snd sprite.offset) * toFloat tileSize)
  in
    div
    ( [ style
        [ "left" => "0"
        , "top" => "0"
        , "transform" => ("translate(" ++ toString left ++ "px," ++ toString top ++ "px)")
        , "position" => "absolute"
        , "overflow" => "hidden"
        , "background-image" => if sprite.src == "" then "none" else ("url(" ++ imagesUrl ++ "/" ++ sprite.src ++ ")")
        , "background-position" => (toString (-frame * width * tileSize) ++ "px 0")
        , "background-repeat" => "no-repeat"
        , "background-size" => (toString (width * sprite.frames * tileSize) ++ "px " ++ (toString (height * tileSize)) ++ "px")
        , "width" => (toString (width * tileSize) ++ "px")
        , "height" => (toString (height * tileSize) ++ "px")
        , "z-index" => (toString layer)
        ]
      ] ++ attributes
    )
    []
