module Sprite (Sprite, Box, Align(..), sort, render) where
import Html.Attributes exposing (style)
import Html exposing (div)


type Align = Top | Bottom


type alias Sprite =
  { size : (Int, Int)
  , offset : (Int, Int)
  , align : Align
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


render : Box -> Html.Html
render box =
  let
    (width, height) = box.sprite.size
    offsetPosition = ( fst box.position + fst box.sprite.offset
                     , snd box.position + snd box.sprite.offset
                     )
    (left, top) =
      case box.sprite.align of
        Top ->
          offsetPosition
        Bottom ->
          (fst offsetPosition, snd offsetPosition - snd box.sprite.size)
  in
    div
    ([ style
       [ "left" => (toString left ++ "px")
       , "top" => (toString top ++ "px")
       , "position" => "absolute"
       , "overflow" => "hidden"
       , "background-image" => ("url(" ++ box.sprite.src ++ ")")
       , "background-position" => (toString (-box.frame * width) ++ "px 0")
       , "background-repeat" => "no-repeat"
       , "background-size" => (toString (width * box.sprite.frames) ++ "px " ++ (toString height) ++ "px")
       , "width" => (toString width ++ "px")
       , "height" => (toString height ++ "px")
       , "z-index" => (toString box.layer)
       ]
    ] ++ box.attributes)
    []
