module AnimationState (AnimationState, animate) where
import Time

type alias AnimationState =
  Maybe { prevClockTime : Time.Time, elapsedFrames : Float }


framesSince : Time.Time -> Time.Time -> Float
framesSince prevTime time =
  min ((time - prevTime) * 60 / 1000) 1.5


animate : Time.Time -> AnimationState -> (Float, AnimationState)
animate time animationState =
  let
    elapsedFrames =
      case animationState of
        Nothing ->
          0
        Just {prevClockTime} ->
          framesSince prevClockTime time
  in
    ( elapsedFrames
    , Just { prevClockTime = time
           , elapsedFrames = elapsedFrames
           }
    )
