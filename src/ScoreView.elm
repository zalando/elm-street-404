module ScoreView (render) where

import Sprite exposing (Sprite)
import Layers exposing (layers)


scoreSprite : Sprite
scoreSprite =
  { size = (1, 1)
  , offset = (0, 0)
  , frames = 13
  , src = "score.png"
  }


digitsList : Int -> List Int
digitsList n =
  let
    nn = n // 10
    r = n % 10
  in
    if nn == 0 && r == 0 then
      []
    else
      r :: (digitsList nn)


render : Int -> Int -> Int -> List Sprite.Box
render score maxLives lives =
  let
    digits = if score == 0 then [0] else digitsList (score * 10)
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
      , position = (x - toFloat number - 1, y)
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
