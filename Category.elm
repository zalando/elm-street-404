module Category (Category(..), random, render) where

import Random
import Array exposing (Array)
import Sprite exposing (Sprite)
import Layers exposing (layers)


type Category
  = Shirt Int
  | Shoes Int
  | Pants Int
  | Scarf Int
  | Placeholder


categories : Array (Int -> Category)
categories = Array.fromList [Pants, Shirt, Shoes, Scarf]


categorySprite : Sprite
categorySprite =
  { size = (1, 1)
  , offset = (0, 0)
  , frames = 13
  , src = "img/category.png"
  }


getFrame : Category -> Int
getFrame category =
  case category of
    Shirt color -> color
    Shoes color -> color + 3
    Pants color -> color + 6
    Scarf color -> color + 9
    Placeholder -> 12


render : (Float, Float) -> Category -> Sprite.Box
render position category =
  { sprite  = categorySprite
  , position  = position
  , frame = getFrame category
  , layer = layers.article
  , attributes = []
  }


random : Random.Seed -> (Category, Random.Seed)
random seed =
  let
    (color, seed') = Random.generate (Random.int 0 3) seed
    (categoryIndex, seed'') = Random.generate (Random.int 0 (Array.length categories)) seed'
  in
    ( (Maybe.withDefault Scarf (Array.get categoryIndex categories)) color
    , seed''
    )
