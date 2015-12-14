module AnimationState (AnimationState, animate) where
import Time exposing (Time)

type alias AnimationState =
  Maybe { prevClockTime : Time, elapsedTime : Time }

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
