module IHopeItWorks (exclude, remove, pickRandom) where
import Random
import Array


remove : (a -> Bool) -> List a -> (Bool, List a)
remove fn list =
  case list of
    [] -> (False, [])
    first :: rest ->
      if fn first then
        (True, rest)
      else
        let
          (found, remainder) = remove fn rest
        in
          (found, first :: remainder)


exclude : List a -> List a -> List a
exclude left right =
  case left of
    [] -> []
    first :: rest ->
      let
        (found, nextRight) = remove ((==) first) right
        nextLeft = exclude rest nextRight
      in
        if found then
          nextLeft
        else
          first :: nextLeft


pickRandom : List a -> Random.Seed -> (Maybe a, Random.Seed)
pickRandom list seed =
  if List.length list == 0 then
    (Nothing, seed)
  else
    let
      (index, seed'') = Random.generate (Random.int 0 (List.length list - 1)) seed
    in
      (Array.get index (Array.fromList list), seed'')
