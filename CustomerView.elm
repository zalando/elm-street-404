module CustomerView where

import Customer exposing (Customer)
import Sprite exposing (Sprite)
import Layers exposing (layers)
import House exposing (House)
import Request exposing (Request)
import Article exposing (Article)
import Category exposing (Category)


size : (Int, Int)
size = (2, 3)


emptySprite : Sprite
emptySprite = Sprite.empty size (0, -1)


sprite : Sprite
sprite =
 { size = size
 , offset = (0, 0)
 , frames = 18
 , src = "img/customers.png"
 }

shoesSprite: Sprite
shoesSprite =
  { size = size
  , offset = (0, 0)
  , frames = 4
  , src = "img/shoes.png"
  }

shirtSprite: Sprite
shirtSprite =
  { size = size
  , offset = (0, 0)
  , frames = 12
  , src = "img/shirts.png"
  }

scarfSprite: Sprite
scarfSprite =
  { size = size
  , offset = (0, 0)
  , frames = 3
  , src = "img/scarves.png"
  }

pantsSprite: Sprite
pantsSprite =
  { size = size
  , offset = (0, 0)
  , frames = 3
  , src = "img/trousers.png"
  }

moodFrameOffset : Int -> Int
moodFrameOffset mood = 2 - mood

shirtFrameOffset : Int -> Customer -> Int
shirtFrameOffset color {happiness, frames} =
  if happiness > 0 then
    color * 3
  else
    color * 3 + 1 + Maybe.withDefault 0 (List.head frames)


frame : Customer -> Int
frame customer =
  customer.typ * 3 +
  (moodFrameOffset customer.happiness)


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
        , frame = frame customer
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
