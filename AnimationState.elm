module AnimationState (AnimationState, AnimatedObject, animateObject, animate, rotateFrames) where
import Time exposing (Time)

type alias AnimationState =
  Maybe { prevClockTime : Time, elapsedTime : Time }

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


animate : Time -> AnimationState -> (Time, AnimationState)
animate time animationState =
  let
    elapsedTime =
      case animationState of
        Nothing ->
          0
        Just {prevClockTime} ->
          min (time - prevClockTime) 25
  in
    ( elapsedTime
    , Just { prevClockTime = time
           , elapsedTime = elapsedTime
           }
    )

rotateFrames : List Int -> List Int
rotateFrames frames =
  case frames of
    frame :: list -> list ++ [frame]
    [] -> []
