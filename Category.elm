module Category (Category(..), getFrame, random) where

import Random
import Array exposing (Array)


type Category
  = Shirt Int
  | Shoes Int
  | Pants Int
  | Scarf Int
  | Placeholder
  | Return


categories : Array (Int -> Category)
categories = Array.fromList [Pants, Shirt, Shoes, Scarf]


getFrame : Category -> Int
getFrame category =
  case category of
    Shirt color -> color
    Shoes color -> color + 3
    Pants color -> color + 6
    Scarf color -> color + 9
    Placeholder -> 12
    Return -> 13


random : Random.Seed -> (Category, Random.Seed)
random seed =
  let
    (color, seed') = Random.generate (Random.int 0 3) seed
    (categoryIndex, seed'') = Random.generate (Random.int 0 (Array.length categories)) seed'
  in
    ( (Maybe.withDefault Scarf (Array.get categoryIndex categories)) color
    , seed''
    )
