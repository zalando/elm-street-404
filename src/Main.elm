import Actions exposing (..)
import Model exposing (Model)
import Html.App as Html
import Update
import View
import Window
import Mouse
import Textures
import AnimationFrame


subscriptions : Model -> Sub Action
subscriptions model =
  Sub.batch
    [ if model.state == Model.Playing then
        AnimationFrame.diffs Actions.Tick
      else
        Sub.none
    , Window.resizes (\{width, height} -> Dimensions (width, height))
    , Mouse.clicks (\{x, y} -> Actions.Click (x, y))
    ]


main : Program Never
main =
  Html.program
    { init =
        ( Model.initial 0 (0, 0) imagesUrl
        , Update.loadImage imagesUrl Textures.Score
        )
    , update = Update.update
    , view = View.view
    , subscriptions = subscriptions
    }


randomSeed : Float
randomSeed = 0


imagesUrl : String
imagesUrl = "img/"


windowDimensions : (Int, Int)
windowDimensions = (500, 500) -- test
