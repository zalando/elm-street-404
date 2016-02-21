module CustomerView (render) where

import Customer exposing (Customer)
import Sprite exposing (Sprite)
import Layers exposing (layers)
import House exposing (House)
import Request exposing (Request)
import Article exposing (Article)
import Category exposing (Category)


size : (Int, Int)
size = (2, 3)


sprite : Sprite
sprite =
  { size = size
  , offset = (0, 0)
  , frames = 18
  , src = "customers.png"
  }


shoesSprite: Sprite
shoesSprite =
  { size = size
  , offset = (0, 0)
  , frames = 4
  , src = "shoes.png"
  }


shirtSprite: Sprite
shirtSprite =
  { size = size
  , offset = (0, 0)
  , frames = 12
  , src = "shirts.png"
  }


scarfSprite: Sprite
scarfSprite =
  { size = size
  , offset = (0, 0)
  , frames = 3
  , src = "scarves.png"
  }


pantsSprite: Sprite
pantsSprite =
  { size = size
  , offset = (0, 0)
  , frames = 3
  , src = "trousers.png"
  }


shirtFrameOffset : Int -> Customer -> Int
shirtFrameOffset color {happiness, frames} =
  if happiness > 0 then
    color * 3
  else
    color * 3 + 1 + Maybe.withDefault 0 (List.head frames)


customerFrameOffset : Customer -> Int
customerFrameOffset {typ, happiness} =
  typ * 3 + 2 - happiness


render : List Request -> List Article -> House -> Customer -> List Sprite.Box
render requests articles house customer =
  let
    categories = (List.map .category articles)
    shirtColor = Category.getColor Category.isShirt categories
    shoesColor = Category.getColor Category.isShoes categories
    pantsColor = Category.getColor Category.isPants categories
    scarfColor = Category.getColor Category.isScarf categories
    position offset = (fst house.position, snd house.position + offset)
  in
    if Customer.isLost customer then
      []
    else
      [ { sprite = sprite
        , position = position 0.001
        , layer = layers.customer
        , frame = customerFrameOffset customer
        , attributes = []
        }
      , { sprite = shirtSprite
        , position = position 0.002
        , layer = layers.customer
        , frame = shirtFrameOffset shirtColor customer
        , attributes = []
        }
      , { sprite = shoesSprite
        , position = position 0.003
        , layer = layers.customer
        , frame = shoesColor
        , attributes = []
        }
      , { sprite = pantsSprite
        , position = position 0.004
        , layer = layers.customer
        , frame = pantsColor
        , attributes = []
        }
      , { sprite = scarfSprite
        , position = position 0.005
        , layer = layers.customer
        , frame = scarfColor
        , attributes = []
        }
      ]
