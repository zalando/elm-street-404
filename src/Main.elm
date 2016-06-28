import Actions exposing (..)
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

subscriptions : Model -> Sub Action
subscriptions model =
  Sub.batch
    [ if model.state == Model.Playing then
        AnimationFrame.diffs Actions.Tick
      else
        Sub.none
    , Window.resizes (\{width, height} -> Dimensions (width, height))
    ]


main : Program Json.Value
main =
  Html.programWithFlags
    { init =
        (\flags ->
          let
            imagesUrl = flags
              |> Json.decodeValue ("imagesUrl" := Json.string)
              |> Result.withDefault "../img/"
            randomSeed = flags
              |> Json.decodeValue ("randomSeed" := Json.int)
              |> Result.withDefault 0
          in
            ( Model.initial randomSeed imagesUrl
            , Cmd.batch
                [ Update.loadImage imagesUrl Textures.Score
                , Task.perform
                    (\_ -> Dimensions (0, 0))
                    (\{width, height} -> Dimensions (width, height))
                    (Process.sleep 100 `Task.andThen` \_ -> Window.size)
                ]
            )
        )
    , update = Update.update
    , view = View.view
    , subscriptions = subscriptions
    }
