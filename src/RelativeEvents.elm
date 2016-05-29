module RelativeEvents exposing
  ( Position
  , onClick
  )

import Json.Decode as Json exposing (Decoder, (:=))
import Html.Events as Events
import Html
import DOM


type alias Position =
  { x : Int
  , y : Int
  }


onClick : (Position -> a) -> Html.Attribute a
onClick tagger =
  Events.on "click" (Json.map tagger relativePosition)


relativePosition : Decoder Position
relativePosition =
  Json.object2
    offsetBy
    ("target" := DOM.boundingClientRect)
    position


offsetBy : DOM.Rectangle ->  { a | x : Int, y : Int } -> { a | x : Int, y : Int }
offsetBy {left, top} position =
  { position
  | x = position.x - round left
  , y = position.y - round top
  }


position : Json.Decoder Position
position =
  Json.object2
    Position
    ("pageX" := Json.int)
    ("pageY" := Json.int)
