import Actions exposing (..)
import Effects
import Html
import Model
import Signal
import StartApp
import Task
import Update
import View


app : { html : Signal Html.Html
      , model : Signal Model.Model
      , tasks : Signal (Task.Task Effects.Never ())
      }
app =
  StartApp.start
    { init = (Model.initial, Effects.tick Actions.Init)
    , update = Update.update
    , view = View.view
    , inputs = []
    }


main : Signal Html.Html
main =
  app.html


port tasks : Signal (Task.Task Effects.Never ())
port tasks =
  app.tasks
