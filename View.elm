module View (view) where
import Actions exposing (Action)
import Html exposing (div, br, Html, text, button)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Model exposing (Model)

(=>) : a -> b -> (a, b)
(=>) = (,)


view : Signal.Address Action -> Model -> Html
view address model =
  div
  [ style
    [ "height" => "590px"
    , "margin" => "auto"
    , "position" => "relative"
    , "width" => "960px"
    , "background" => "lightgrey"
    ]
  ]
  [ text ("404 Elm Street")
  ]
