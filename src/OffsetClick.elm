module OffsetClick exposing
  ( Position
  , onClick
  )

import Json.Decode as Json exposing (Decoder, (:=))
import Html.Events as Events
import Html
import Result exposing (Result)
import Native.Offset


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
    (Json.customDecoder ("target" := Json.value) Native.Offset.offset)
    position


offsetBy : Position -> Position -> Position
offsetBy {x, y} position =
  { x = position.x - x
  , y = position.y - y
  }


offset : Json.Value -> Result String Position
offset =
  Native.Offset.offset


position : Json.Decoder Position
position =
  Json.object2
    Position
    ("pageX" := Json.int)
    ("pageY" := Json.int)
