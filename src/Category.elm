module Category exposing
  ( Category(..)
  , getFrame
  , random
  , color
  , getColor
  , isShirt
  , isShoes
  , isPants
  , isScarf
  , isSame
  )

import Random
import IHopeItWorks


type Category
  = Shirt Int
  | Shoes Int
  | Pants Int
  | Scarf Int
  | Placeholder
  | Return


categories : List (Int -> Category)
categories = [Pants, Shirt, Shoes, Scarf]


getFrame : Category -> Int
getFrame category =
  case category of
    Shirt color -> color
    Shoes color -> color + 3
    Pants color -> color + 6
    Scarf color -> color + 9
    Placeholder -> 12
    Return -> 13


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


getColor : (Category -> Bool) -> List Category -> Maybe Int
getColor fn categories =
  case IHopeItWorks.find fn categories of
    Just shirt -> Just (color shirt)
    _ -> Nothing


random : Random.Generator Category
random  =
  Random.map2
    (<|)
    (Random.map (Maybe.withDefault Scarf) (IHopeItWorks.pickRandom categories))
    (Random.int 0 2)
