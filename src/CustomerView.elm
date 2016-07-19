module CustomerView exposing (render)

import Customer exposing (Customer)
import Box exposing (Box)
import Textures
import Layers exposing (layers)
import MapObject exposing (MapObject)
import Request exposing (Request)
import Article exposing (Article)
import Category exposing (Category)
import Actions


shirtFrameOffset : Customer -> Int -> Int
shirtFrameOffset {happiness, frames} color =
  if happiness > 0 then
    color * 3
  else
    color * 3 + 1 + Maybe.withDefault 0 (List.head frames)


customerFrameOffset : Customer -> Int
customerFrameOffset {typ, happiness, frames} =
  case happiness of
    0 ->
      typ * 6 + 2 + Maybe.withDefault 0 (List.head frames)
    1 ->
      typ * 6 + 1
    _ ->
      typ * 6


render : List Request -> List Article -> MapObject -> Customer -> List Box
render requests articles ({position} as house) customer =
  let
    categories = List.map .category articles
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
    if Customer.isLost customer then
      []
    else
      renderColor scarfColor 5 Textures.Scarves ++
      renderColor pantsColor 4 Textures.Trousers ++
      renderColor shoesColor 3 Textures.Shoes ++
      renderColor shirtColor 2 Textures.Shirts ++
      [ Box.textured Textures.Customers position (customerFrameOffset customer) (layers.obstacle, 1)
      , Box.clickable (2, 3) (0, 0) position (layers.click, 0) (Actions.ClickMapObject house)
      ]
