module DigitsView (render) where

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


render : (Float, Float) -> Int -> List Sprite.Box
render (x, y) value =
  let
    digits = if value == 0 then [0] else digitsList value
    renderDigit number digit =
      { sprite = scoreSprite
      , position = (x - toFloat number - 1, y)
      , layer = (layers.customer, 0)
      , frame = digit
      , attributes = []
      }
  in
    (List.indexedMap renderDigit digits)
