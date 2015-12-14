module Update (update) where
import Model exposing (..)
import Actions exposing (..)
import Effects exposing (Effects)
import Time exposing (Time)
import Random


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Init time ->
      ( {model | seed = Random.initialSeed (floor time)}
      , Effects.tick (always Start)
      )
    Start ->
      ({model | state = Playing }, Effects.tick Tick)
    Tick time ->
      if model.state == Playing then
        (Model.animate time animate model, Effects.tick Tick)
      else
        ({model | animationState = Nothing}, Effects.none)


animate : Float -> Model -> Model
animate frames model = model
