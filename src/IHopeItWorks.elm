module IHopeItWorks exposing (exclude, remove, pickRandom, find)

import Random


{-| find the first element in the list that matches the given predicate -}
find : (a -> Bool) -> List a -> Maybe a
find fn list =
  case list of
    [] -> Nothing
    el :: rest ->
      if fn el then
        Just el
      else
        find fn rest


remove' : (a -> Bool) -> List a -> (Bool, List a)
remove' fn list =
  case list of
    [] -> (False, [])
    first :: rest ->
      if fn first then
        (True, rest)
      else
        let
          (found, remainder) = remove' fn rest
        in
          (found, first :: remainder)


{-| remove only the first match from the list -}
remove : (a -> Bool) -> List a -> List a
remove fn = remove' fn >> snd


{-| exlude right list items from the given left list -}
exclude : List a -> List a -> List a
exclude left right =
  case left of
    [] -> []
    first :: rest ->
      let
        (found, nextRight) = remove' ((==) first) right
        nextLeft = exclude rest nextRight
      in
        if found then
          nextLeft
        else
          first :: nextLeft


{-| generate random element from the list -}
pickRandom : List a -> Random.Generator (Maybe a)
pickRandom list =
  Random.map
    (\index -> List.head (List.drop index list))
    (Random.int 0 (List.length list - 1))
