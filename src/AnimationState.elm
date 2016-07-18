module AnimationState exposing
  ( AnimatedObject
  , animateObject
  , rotateFrames
  )

import Time exposing (Time)


type alias AnimatedObject a =
  { a
  | elapsed : Time
  , timeout : Time
  }


{-| executes animationFunc every time timeout is reached -}
animateObject : Time -> (AnimatedObject a -> AnimatedObject a) -> AnimatedObject a -> AnimatedObject a
animateObject elapsed animationFunc state =
  let
    elapsed' = state.elapsed + elapsed
  in
    if elapsed' > state.timeout then
      animationFunc {state | elapsed = elapsed' - state.timeout}
    else
      {state | elapsed = elapsed'}


{-| takes the fist frame from list and puts it in the end -}
rotateFrames : {a | frames : List Int} -> {a | frames : List Int}
rotateFrames obj =
  case obj.frames of
    [] -> obj
    frame :: list -> {obj | frames = list ++ [frame]}
