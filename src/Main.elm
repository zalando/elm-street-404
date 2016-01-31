import Actions exposing (..)
import Effects
import Html
import Model
import Signal
import StartApp
import Task
import Update
import View
import Window


app : { html : Signal Html.Html
      , model : Signal Model.Model
      , tasks : Signal (Task.Task Effects.Never ())
      }
app =
  StartApp.start
    { init = (Model.initial windowDimensions, Effects.tick Actions.Init)
    , update = Update.update
    , view = View.view
    , inputs = [Signal.map Actions.Dimensions Window.dimensions]
    }


main : Signal Html.Html
main =
  app.html


port windowDimensions : (Int, Int)


port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks
