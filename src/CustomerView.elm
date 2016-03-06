module CustomerView (render) where

import Customer exposing (Customer)
import Box exposing (Box)
import Textures
import Layers exposing (layers)
import MapObject exposing (MapObject)
import Request exposing (Request)
import Article exposing (Article)
import Category exposing (Category)


shirtFrameOffset : Int -> Customer -> Int
shirtFrameOffset color {happiness, frames} =
  if happiness > 0 then
    color * 3
  else
    color * 3 + 1 + Maybe.withDefault 0 (List.head frames)


customerFrameOffset : Customer -> Int
customerFrameOffset {typ, happiness} =
  typ * 3 + 2 - happiness


render : List Request -> List Article -> MapObject -> Customer -> List Box
render requests articles ({position} as house) customer =
  let
    categories = List.map .category articles
    shirtColor = Maybe.withDefault 3 (Category.getColor Category.isShirt categories)
    shoesColor = Maybe.withDefault 3 (Category.getColor Category.isShoes categories)
    pantsColor = Category.getColor Category.isPants categories
    scarfColor = Category.getColor Category.isScarf categories
    renderColor maybeColor layer sprite =
      case maybeColor of
        Just color ->
          [Box.textured sprite position color (layers.obstacle, layer)]
        Nothing ->
          []
  in
    if Customer.isLost customer then
      []
    else
      renderColor pantsColor 4 Textures.Trousers ++
      renderColor scarfColor 5 Textures.Scarves ++
      [ Box.textured Textures.Shoes position shoesColor (layers.obstacle, 3)
      , Box.textured Textures.Customers position (customerFrameOffset customer) (layers.obstacle, 1)
      , Box.textured Textures.Shirts position (shirtFrameOffset shirtColor customer) (layers.obstacle, 2)
      ]
