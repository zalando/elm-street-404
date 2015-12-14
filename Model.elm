module Model (Model, initial, animate, State(..)) where
import Random
import Time
import AnimationState

type State = Paused | Playing | Stopped


type alias Model =
  { animationState : AnimationState.AnimationState
  , state : State
  , seed : Random.Seed
  }


initial : Model
initial =
  { animationState = Nothing
  , state = Stopped
  , seed = Random.initialSeed 0
  }


animate : Time.Time -> (Float -> Model -> Model) -> Model -> Model
animate time func model =
  let
    (elapsedFrames, animationState) = AnimationState.animate time model.animationState
  in
    func elapsedFrames {model | animationState = animationState}
