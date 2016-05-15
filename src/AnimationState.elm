module AnimationState exposing
  ( AnimatedObject
  , Dispatcher
  , dispatcher
  , animateDispatcher
  , dispatcherActions
  , animateObject
  , rotateFrames
  )

import Time exposing (Time)


type alias AnimatedObject a =
  { a
  | elapsed : Time
  , timeout : Time
  }


type alias Generator a = AnimatedObject {active : Bool, action : a}


type alias Dispatcher a = List (Generator a)


dispatcher : List (Time, a) -> Dispatcher a
dispatcher =
  List.map (uncurry generator)


generator : Time -> a -> Generator a
generator timeout action =
  { elapsed = 0
  , timeout = timeout
  , action = action
  , active = False
  }


animateGenerator : Time -> Generator a -> Generator a
animateGenerator elapsed generator =
  animateObject
    elapsed
    (\g -> {g | active = True})
    {generator | active = False}


animateDispatcher : Time -> Dispatcher a -> Dispatcher a
animateDispatcher =
  animateGenerator >> List.map


dispatcherActions : Dispatcher a -> List a
dispatcherActions =
  List.filter .active >> List.map .action


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
