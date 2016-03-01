module ScoreView (render) where

import Sprite
import Layers exposing (layers)
import DigitsView


render : (Int, Int) -> Int -> Int -> Int -> List Sprite.Box
render (width, height) score maxLives lives =
  let
    x = toFloat width - 2
    y = 1

    renderLife number _ =
      Sprite.box
        Sprite.Score
        (toFloat (maxLives - number), y)
        (if number >= (maxLives - lives) then 11 else 12)
        (layers.bubble, 0)

  in
    ( Sprite.box
        Sprite.Score
        (x, y)
        10
        (layers.bubble, 0)
    )
    :: (DigitsView.render (x, y) (score * 10))
    ++ (List.indexedMap renderLife (List.repeat maxLives True))
