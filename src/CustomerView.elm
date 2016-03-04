module CustomerView (render) where

import Customer exposing (Customer)
import Sprite
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


render : List Request -> List Article -> MapObject -> Customer -> List Sprite.Box
render requests articles house customer =
  let
    categories = (List.map .category articles)
    shirtColor = Maybe.withDefault 3 (Category.getColor Category.isShirt categories)
    shoesColor = Maybe.withDefault 3 (Category.getColor Category.isShoes categories)
    pantsColor = Category.getColor Category.isPants categories
    scarfColor = Category.getColor Category.isScarf categories
    renderColor maybeColor layer sprite =
      case maybeColor of
        Just color ->
          [ Sprite.box
              sprite
              house.position
              color
              (layers.obstacle, layer)
          ]
        Nothing ->
          []
  in
    if Customer.isLost customer then
      []
    else
      List.concat
      [ renderColor pantsColor 4 Sprite.Trousers
      , renderColor scarfColor 5 Sprite.Scarves
      , [ Sprite.box
            Sprite.Shoes
            house.position
            shoesColor
            (layers.obstacle, 3)
        , Sprite.box
            Sprite.Customers
            house.position
            (customerFrameOffset customer)
            (layers.obstacle, 1)
        , Sprite.box
            Sprite.Shirts
            house.position
            (shirtFrameOffset shirtColor customer)
            (layers.obstacle, 2)
        ]
      ]
