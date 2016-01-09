module ScoreView (render) where

import Sprite exposing (Sprite)
import Layers exposing (layers)
import IHopeItWorks

scoreSprite : Sprite
scoreSprite =
  { size = (1, 1)
  , offset = (0, 0)
  , frames = 13
  , src = "img/score.png"
  }


render : Int -> Int -> Int -> List Sprite.Box
render score maxLives lives =
  let
    digits' = List.reverse (IHopeItWorks.digits (score * 10))
    digits = if digits' == [] then [0] else digits'
    x = 22
    y = 1

    renderLife number _ =
      { sprite = scoreSprite
      , position = (9 + toFloat (maxLives - number), y)
      , layer = layers.customer
      , frame = if number >= (maxLives - lives) then 11 else 12
      , attributes = []
      }

    renderDigit number digit =
      { sprite = scoreSprite
      , position = (x + toFloat (number - List.length digits), y)
      , layer = layers.customer
      , frame = digit
      , attributes = []
      }
  in
    { sprite = scoreSprite
    , position = (x, y)
    , layer = layers.customer
    , frame = 10
    , attributes = []
    }
    :: (List.indexedMap renderDigit digits)
    ++ (List.indexedMap renderLife (List.repeat maxLives True))
