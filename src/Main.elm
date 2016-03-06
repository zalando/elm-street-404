import Actions exposing (..)
import Effects exposing (Never)
import Html exposing (Html)
import Model exposing (Model)
import StartApp exposing (App)
import Task exposing (Task)
import Update
import View
import Window
import Textures


app : App Model
app =
  StartApp.start
    { init =
        ( Model.initial randomSeed windowDimensions imagesUrl
        , (Update.loadImage imagesUrl) Textures.Score
        )
    , update = Update.update
    , view = View.view
    , inputs = [Signal.map Actions.Dimensions Window.dimensions]
    }


main : Signal Html
main = app.html


port randomSeed : Int


port imagesUrl : String


port windowDimensions : (Int, Int)


port tasks : Signal (Task Never ())
port tasks = app.tasks
