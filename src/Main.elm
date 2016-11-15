port module Main exposing (..)

import Actions exposing (Action(..))
import Model exposing (Model)
import Html
import Update
import View
import Window
import Textures
import AnimationFrame
import Task
import Process
import Json.Decode as Json
import Time
import Keyboard


port suspend : (Bool -> msg) -> Sub msg


port restore : (Bool -> msg) -> Sub msg


subscriptions : Model -> Sub Action
subscriptions model =
    case model.state of
        Model.Suspended _ ->
            restore (\_ -> Actions.Restore)

        _ ->
            Sub.batch
                [ if model.state == Model.Playing || model.state == Model.Lost || model.state == Model.Won then
                    AnimationFrame.diffs Tick
                  else
                    Sub.none
                , Window.resizes Dimensions
                , if model.embed then
                    Keyboard.downs escapeToSuspend
                  else
                    Sub.none
                , suspend (\_ -> Suspend)
                , model.events
                    |> List.map (\( time, action ) -> Time.every time (\_ -> Event action))
                    |> Sub.batch
                ]


main : Program Json.Value Model Action
main =
    Html.programWithFlags
        { init =
            (\flags ->
                let
                    imagesUrl =
                        flags
                            |> Json.decodeValue (Json.field "imagesUrl" Json.string)
                            |> Result.withDefault "../img"

                    randomSeed =
                        flags
                            |> Json.decodeValue (Json.field "randomSeed" Json.int)
                            |> Result.withDefault 0

                    embed =
                        flags
                            |> Json.decodeValue (Json.field "embed" Json.bool)
                            |> Result.withDefault False

                    devicePixelRatio =
                        flags
                            |> Json.decodeValue (Json.field "devicePixelRatio" Json.float)
                            |> Result.withDefault 1
                in
                    ( Model.initial randomSeed imagesUrl embed devicePixelRatio
                    , Cmd.batch
                        [ Update.loadImage imagesUrl Textures.Score
                        , Task.perform
                            Dimensions
                            (Process.sleep 100 |> Task.andThen (\_ -> Window.size))
                        ]
                    )
            )
        , update = Update.update
        , view = View.view
        , subscriptions = subscriptions
        }


escapeToSuspend : Int -> Action
escapeToSuspend keyCode =
    case keyCode of
        27 ->
            Actions.Suspend

        _ ->
            Actions.NoOp
