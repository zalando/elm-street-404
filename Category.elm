module Category (Category(..), getFrame, random, color) where

import Random
import Array exposing (Array)


type Category
  = Shirt Int
  | Shoes Int
  | Pants Int
  | Scarf Int
  | Placeholder
  | Return
  | Empty


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
    Empty -> 14


color : Category -> Int
color category =
  case category of
   Shirt color -> color
   Shoes color -> color
   Pants color -> color
   Scarf color -> color
   _ -> 0


random : Random.Seed -> (Category, Random.Seed)
random seed =
  let
    (color, seed') = Random.generate (Random.int 0 2) seed
    (categoryIndex, seed'') = Random.generate (Random.int 0 (Array.length categories - 1)) seed'
  in
    ( (Maybe.withDefault Scarf (Array.get categoryIndex categories)) color
    , seed''
    )
