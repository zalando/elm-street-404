module Category (Category(..), getFrame, random, color, getColor, isShirt, isShoes, isPants, isScarf, isSame) where

import Random
import Array exposing (Array)
import IHopeItWorks

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


isShirt : Category -> Bool
isShirt category =
  case category of
    Shirt _ -> True
    _ -> False


isShoes : Category -> Bool
isShoes category =
  case category of
    Shoes _ -> True
    _ -> False


isPants : Category -> Bool
isPants category =
  case category of
    Pants _ -> True
    _ -> False


isScarf : Category -> Bool
isScarf category =
  case category of
    Scarf _ -> True
    _ -> False


baseCategory : Category -> Int
baseCategory category =
  case category of
    Shirt _ -> 1
    Shoes _ -> 2
    Pants _ -> 3
    Scarf _ -> 4
    Placeholder -> 5
    Return -> 6
    Empty -> 7


isSame : Category -> Category -> Bool
isSame cat1 cat2 =
  baseCategory cat1 == baseCategory cat2


color : Category -> Int
color category =
  case category of
   Shirt color -> color
   Shoes color -> color
   Pants color -> color
   Scarf color -> color
   _ -> 0


getColor : (Category -> Bool) -> List Category -> Int
getColor fn categories =
  let
    maybeCategory = IHopeItWorks.first fn categories
  in
    case maybeCategory of
      Just shirt -> color shirt
      _ -> 3


random : Random.Seed -> (Category, Random.Seed)
random seed =
  let
    (color, seed') = Random.generate (Random.int 0 2) seed
    (categoryIndex, seed'') = Random.generate (Random.int 0 (Array.length categories - 1)) seed'
  in
    ( Scarf 1 -- (Maybe.withDefault Scarf (Array.get categoryIndex categories)) color
    , seed''
    )
