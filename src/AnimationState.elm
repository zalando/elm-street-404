module AnimationState (AnimationState, AnimatedObject, Generator, generator, animateGenerator, animateObject, animate, rotateFrames) where

import Time exposing (Time)


type alias AnimationState =
  Maybe { prevClockTime : Time, elapsed : Time }


type alias AnimatedObject a =
  { a
  | elapsed : Time
  , timeout : Time
  }


type alias Generator =
  AnimatedObject {active : Bool}


generator : Time -> Generator
generator timeout =
  { elapsed = 0
  , timeout = timeout
  , active = False
  }


animateGenerator : Time -> Generator -> Generator
animateGenerator elapsed generator =
  animateObject
    elapsed
    (\g -> {g | active = True})
    {generator | active = False}


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


{-| calculates time difference between two frames -}
animate : Time -> AnimationState -> (Time, AnimationState)
animate time animationState =
  let
    elapsed =
      case animationState of
        Nothing ->
          0
        Just {prevClockTime} ->
          min (time - prevClockTime) 25
  in
    ( elapsed
    , Just { prevClockTime = time
           , elapsed = elapsed
           }
    )


{-| takes the fist frame from list and puts it in the end -}
rotateFrames : {a | frames : List Int} -> {a | frames : List Int}
rotateFrames obj =
  case obj.frames of
    [] -> obj
    frame :: list -> {obj | frames = list ++ [frame]}
