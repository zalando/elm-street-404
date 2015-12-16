module Sprite (Sprite, Box, sort, render) where
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


(=>) : a -> b -> (a, b)
(=>) = (,)


sort : List Box -> List Box
sort boxes =
  List.sortBy (\box -> snd box.position) boxes


render : Int -> Box -> Html.Html
render tileSize {sprite, position, frame, layer, attributes} =
  let
    (width, height) = sprite.size
    left = round ((fst position + fst sprite.offset) * toFloat tileSize)
    top = round ((snd position + snd sprite.offset) * toFloat tileSize)
  in
    div
    ([ style
       [ "left" => (toString left ++ "px")
       , "top" => (toString top ++ "px")
       , "position" => "absolute"
       , "overflow" => "hidden"
       , "background-image" => ("url(" ++ sprite.src ++ ")")
       , "background-position" => (toString (-frame * width * tileSize) ++ "px 0")
       , "background-repeat" => "no-repeat"
       , "background-size" => (toString (width * sprite.frames * tileSize) ++ "px " ++ (toString (height * tileSize)) ++ "px")
       , "width" => (toString (width * tileSize) ++ "px")
       , "height" => (toString (height * tileSize) ++ "px")
       , "z-index" => (toString layer)
       ]
    ] ++ attributes)
    []
