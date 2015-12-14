module Model (Model, initial, animate, State(..), animateFountain) where
import Random
import Time exposing (Time)
import AnimationState

type State = Paused | Playing | Stopped


type alias Fountain =
  { elapsed : Time
  , frames : List Int
  }


type alias Model =
  { animationState : AnimationState.AnimationState
  , state : State
  , seed : Random.Seed
  , fountain : Fountain
  , tileSize : Int
  }


initial : Model
initial =
  { animationState = Nothing
  , state = Stopped
  , seed = Random.initialSeed 0
  , fountain = Fountain 0 [0, 1, 2, 3]
  , tileSize = 40
  }


rotateFrames : List Int -> List Int
rotateFrames frames =
  case frames of
    frame :: list -> list ++ [frame]
    [] -> []


animate : Time -> (Time -> Model -> Model) -> Model -> Model
animate time animationFunc model =
  let
    (elapsed, animationState) = AnimationState.animate time model.animationState
  in
    animationFunc elapsed {model | animationState = animationState}


animateFountain : Time -> Fountain -> Fountain
animateFountain time fountain =
  let
    updateFountain fountain =
      { fountain | frames = rotateFrames fountain.frames }
  in
    animateObject 150 time updateFountain fountain


type alias AnimatedObject a =
  { a | elapsed: Time }


animateObject : Time -> Time -> (AnimatedObject a -> AnimatedObject a) -> AnimatedObject a -> AnimatedObject a
animateObject limit elapsed animationFunc state =
  let
    elapsed' = state.elapsed + elapsed
  in
    if elapsed' > limit then
      animationFunc {state | elapsed = elapsed' - limit}
    else
      {state | elapsed = elapsed'}
