module ClickableView (render) where

import Box
import Html exposing (div)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Signal exposing (Address)
import Actions exposing (Action)


(=>) : a -> b -> (a, b)
(=>) = (,)


renderItem : Address Action -> Int -> Box.ClickableBoxData -> Html.Html
renderItem address tileSize {position, size, offset, onClickAction} =
  div
  ( [ style
      [ "left" => (toString ((fst position + fst offset) * toFloat tileSize) ++ "px")
      , "top" => (toString ((snd position + snd offset) * toFloat tileSize) ++ "px")
      , "position" => "absolute"
      , "width" => (toString (fst size * toFloat tileSize) ++ "px")
      , "height" => (toString (snd size * toFloat tileSize) ++ "px")
      ]
    , onClick address onClickAction
    ]
  )
  []


render : Address Action -> Int -> List Box.ClickableBoxData -> List Html.Html
render address tileSize =
  List.map (renderItem address tileSize)
