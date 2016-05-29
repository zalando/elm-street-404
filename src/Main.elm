import Actions exposing (..)
import Model exposing (Model)
import Html.App as Html
import Update
import View
import Window
import Textures
import AnimationFrame
import Task


subscriptions : Model -> Sub Action
subscriptions model =
  Sub.batch
    [ if model.state == Model.Playing then
        AnimationFrame.diffs Actions.Tick
      else
        Sub.none
    , Window.resizes (\{width, height} -> Dimensions (width, height))
    ]


main : Program Never
main =
  Html.program
    { init =
        ( Model.initial randomSeed imagesUrl
        , Cmd.batch
            [ Update.loadImage imagesUrl Textures.Score
            , Task.perform
                (always Dimensions (0, 0))
                (\{width, height} -> Dimensions (width, height))
                Window.size
            ]
        )
    , update = Update.update
    , view = View.view
    , subscriptions = subscriptions
    }


randomSeed : Int
randomSeed = 0


imagesUrl : String
imagesUrl = "../img/"
