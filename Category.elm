module Category (Category(..), random) where

import Random
import Array exposing (Array)


type Category
  = Pants Int
  | Shirt Int
  | Shoes Int
  | Scarf Int


categories : Array (Int -> Category)
categories = Array.fromList [Pants, Shirt, Shoes, Scarf]

{-
box : Category -> Sprite.Box
box category =
-}

random : Random.Seed -> (Category, Random.Seed)
random seed =
  let
    (color, seed') = Random.generate (Random.int 0 3) seed
    (categoryIndex, seed'') = Random.generate (Random.int 0 (Array.length categories)) seed'
  in
    ( (Maybe.withDefault Scarf (Array.get categoryIndex categories)) color
    , seed''
    )
