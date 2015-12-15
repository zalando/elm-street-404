module Sprite (Sprite, Box, sort, render) where
import Html.Attributes exposing (style)
import Html exposing (div)



type alias Sprite =
  { size : (Int, Int)
  , offset : (Int, Int)
  , frames : Int
  , src : String
  }


type alias Box =
  { position : (Int, Int)
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
render tileSize box =
  let
    (width, height) = box.sprite.size
    (left, top) = ( fst box.position + fst box.sprite.offset
                  , snd box.position + snd box.sprite.offset
                  )
  in
    div
    ([ style
       [ "left" => (toString (left * tileSize) ++ "px")
       , "top" => (toString (top * tileSize) ++ "px")
       , "position" => "absolute"
       , "overflow" => "hidden"
       , "background-image" => ("url(" ++ box.sprite.src ++ ")")
       , "background-position" => (toString (-box.frame * width * tileSize) ++ "px 0")
       , "background-repeat" => "no-repeat"
       , "background-size" => (toString (width * box.sprite.frames * tileSize) ++ "px " ++ (toString (height * tileSize)) ++ "px")
       , "width" => (toString (width * tileSize) ++ "px")
       , "height" => (toString (height * tileSize) ++ "px")
       , "z-index" => (toString box.layer)
       , "transform" => "translateZ(0)"
       ]
    ] ++ box.attributes)
    []
