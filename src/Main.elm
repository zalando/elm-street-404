port module Main exposing (..)

import Actions exposing (Action(..))
import Model exposing (Model)
import Html.App as Html
import Update
import View
import Window
import Textures
import AnimationFrame
import Task
import Process
import Json.Decode as Json exposing ((:=))
import Time


port suspend : (Bool -> msg) -> Sub msg


port restore : (Bool -> msg) -> Sub msg


subscriptions : Model -> Sub Action
subscriptions model =
  case model.state of
    Model.Suspended _ ->
      restore (\_ -> Actions.Restore)
    _ ->
      Sub.batch
        [ if model.state == Model.Playing then
            AnimationFrame.diffs Tick
          else
            Sub.none
        , Window.resizes Dimensions
        , suspend (\_ -> Suspend)
        , model.events
            |> List.map (\(time, action) -> Time.every time (\_ -> Event action))
            |> Sub.batch
        ]


main : Program Json.Value
main =
  Html.programWithFlags
    { init =
        (\flags ->
          let
            imagesUrl = flags
              |> Json.decodeValue ("imagesUrl" := Json.string)
              |> Result.withDefault "../img"
            randomSeed = flags
              |> Json.decodeValue ("randomSeed" := Json.int)
              |> Result.withDefault 0
            embed = flags
              |> Json.decodeValue ("embed" := Json.bool)
              |> Result.withDefault False
          in
            ( Model.initial randomSeed imagesUrl embed
            , Cmd.batch
                [ Update.loadImage imagesUrl Textures.Score
                , Task.perform
                    identity
                    Dimensions
                    (Process.sleep 100 `Task.andThen` \_ -> Window.size)
                ]
            )
        )
    , update = Update.update
    , view = View.view
    , subscriptions = subscriptions
    }
