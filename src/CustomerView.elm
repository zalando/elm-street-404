module CustomerView exposing (render)

import Customer exposing (Customer, Location(..))
import Box exposing (Box)
import Textures
import Layers exposing (layers)
import Article exposing (Article)
import Category exposing (Category)


shirtFrameOffset : Customer -> Int -> Int
shirtFrameOffset {happiness, frame} color =
  if happiness > 0 then
    color * 3
  else
    color * 3 + 1 + frame


customerFrameOffset : Customer -> Int
customerFrameOffset {typ, happiness, frame} =
  case happiness of
    0 ->
      typ * 6 + 2 + frame
    1 ->
      typ * 6 + 1
    _ ->
      typ * 6


render : List Article -> (Float, Float) -> Customer -> List Box
render articles position customer =
  let
    categories = articles
      |> List.filter (\{state} -> state == Article.Delivered customer)
      |> List.map .category
    shirtColor = Category.getColor Category.isShirt categories
      |> Maybe.map (shirtFrameOffset customer)
    shoesColor = Category.getColor Category.isShoes categories
    pantsColor = Category.getColor Category.isPants categories
    scarfColor = Category.getColor Category.isScarf categories
    renderColor maybeColor layer sprite =
      case maybeColor of
        Just color ->
          [Box.textured sprite position color (layers.obstacle, layer)]
        Nothing ->
          []
  in
    renderColor scarfColor 5 Textures.Scarves ++
    renderColor pantsColor 4 Textures.Trousers ++
    renderColor shoesColor 3 Textures.Shoes ++
    renderColor shirtColor 2 Textures.Shirts ++
    [ Box.textured Textures.Customers position (customerFrameOffset customer) (layers.obstacle, 1)
    ]
