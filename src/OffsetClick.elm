module OffsetClick
    exposing
        ( Position
        , onClick
        )

import Json.Decode as Json exposing (Decoder)
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
    Json.map2
        offsetBy
        (Json.field "target" Json.value
            |> Json.andThen
                (\value ->
                    case Native.Offset.offset value of
                        Ok val ->
                            Json.succeed val

                        Err err ->
                            Json.fail err
                )
        )
        position


offsetBy : Position -> Position -> Position
offsetBy { x, y } position =
    { x = position.x - x
    , y = position.y - y
    }


offset : Json.Value -> Result String Position
offset =
    Native.Offset.offset


position : Json.Decoder Position
position =
    Json.map2
        Position
        (Json.field "pageX" Json.int)
        (Json.field "pageY" Json.int)
