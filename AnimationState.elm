module AnimationState (AnimationState, AnimatedObject, animateObject, animate, rotateFrames) where
import Time exposing (Time)

type alias AnimationState =
  Maybe { prevClockTime : Time, elapsed : Time }

type alias AnimatedObject a =
  { a | elapsed: Time }


animateObject : Time -> Time -> (AnimatedObject a -> AnimatedObject a) -> AnimatedObject a -> AnimatedObject a
animateObject timeout elapsed animationFunc state =
  let
    elapsed' = state.elapsed + elapsed
  in
    if elapsed' > timeout then
      animationFunc {state | elapsed = elapsed' - timeout}
    else
      {state | elapsed = elapsed'}


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

rotateFrames : List Int -> List Int
rotateFrames frames =
  case frames of
    frame :: list -> list ++ [frame]
    [] -> []
