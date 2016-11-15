module AnimationState
    exposing
        ( AnimatedObject
        , animateFrame
        )

import Time exposing (Time)


type alias AnimatedObject a =
    { a
        | elapsed : Time
        , timeout : Time
        , frame : Int
    }


{-| incrtemens frame every time timeout is reached
-}
animateFrame : Int -> Time -> AnimatedObject a -> AnimatedObject a
animateFrame frames elapsed state =
    let
        elapsed_ =
            state.elapsed + elapsed
    in
        if elapsed_ > state.timeout then
            { state
                | elapsed = elapsed_ - state.timeout
                , frame = (state.frame + 1) % frames
            }
        else
            { state | elapsed = elapsed_ }
