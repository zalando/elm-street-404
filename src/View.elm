module View exposing (view)

import Actions exposing (Action)
import Html exposing (div, br, Html, text, button)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick, onMouseOver, onMouseLeave)
import Model exposing (Model)
import PathView
import WebGLView
import OffsetClick
import Box exposing (Box)


(=>) : a -> b -> (a, b)
(=>) = (,)


debug : Model -> Html Action
debug model =
  div
    [ style
        [ "background" => "linear-gradient(0deg, #000000 0, rgba(0,0,0,0) 2px, rgba(0,0,0,0) 100%), linear-gradient(90deg, #000000 0, rgba(0,0,0,0) 2px, rgba(0,0,0,0) 100%)"
        , "background-origin" => "padding-box"
        , "background-clip" => "border-box"
        , "background-size" => (toString model.tileSize ++ "px " ++ toString model.tileSize ++ "px")
        , "position" => "absolute"
        , "left" => "0"
        , "top" => "0"
        , "width" => "100%"
        , "height" => "100%"
        ]
    ]
    []


view : Model -> Html Action
view model =
  let
    (texturedBoxes, _) = Box.split model.boxes
    mapWidth = fst model.gridSize * model.tileSize
    mapHeight = snd model.gridSize * model.tileSize
    screenWidth = max (fst model.dimensions) mapWidth
    screenHeight = max (snd model.dimensions) mapHeight
  in
    case model.state of
      Model.Suspended _ ->
        text ""
      _ ->
        div
          [ style
              [ "background-image" => ("url(" ++ model.imagesUrl ++ "/bg-tile.jpg" ++ ")")
              , "background-size" => "560px 560px"
              , "background-position" => "50% 50%"
              , "position" => "relative"
              , "width" => (toString screenWidth ++ "px")
              , "height" => (toString screenHeight ++ "px")
              ]
          ]
          ( div
              [ style
                [ "position" => "absolute"
                , "width" => (toString mapWidth ++ "px")
                , "height" => (toString mapHeight ++ "px")
                , "left" => (toString ((screenWidth - mapWidth) // 2) ++ "px")
                , "top" => (toString ((screenHeight - mapHeight) // 2) ++ "px")
                , "-webkit-tap-highlight-color" => "transparent"
                ]
              , OffsetClick.onClick Actions.Click
              ]
              [ PathView.render model.gridSize model.tileSize model.deliveryPerson.route
              , WebGLView.render model.gridSize model.tileSize model.textures texturedBoxes
              ]
            :: if model.embed then [closeButton model.closeButtonActive (toFloat model.tileSize) (model.imagesUrl ++ "/close.png")] else []
          )


closeButton : Bool -> Float -> String -> Html Action
closeButton active size url =
  button
    [ onClick Actions.Suspend
    , onMouseLeave (Actions.HoverCloseButton False)
    , onMouseOver (Actions.HoverCloseButton True)
    , style
        [ "position" => "absolute"
        , "right" => "0px"
        , "top" => "0px"
        , "border" => "none"
        , "padding" => "0"
        , "margin" => "0"
        , "width" => (toString (round (size * 110 / 80)) ++ "px")
        , "height" => (toString (round (size * 110 / 97)) ++ "px")
        , "background-color" => "transparent"
        , "background-image" => ("url(" ++ url ++ ")")
        , "background-size" => "200% 100%"
        , "background-position" => (if active then "-100% 0" else "0 0")
        , "outline" => "none"
        , "cursor" => "pointer"
        , "touch-action" => "manipulation"
        ]
    ]
    []
