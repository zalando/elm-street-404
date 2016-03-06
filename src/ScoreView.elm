module ScoreView (render) where

import Box exposing (Box)
import Textures
import Layers exposing (layers)
import DigitsView


render : (Int, Int) -> Int -> Int -> Int -> List Box
render (width, height) score maxLives lives =
  let
    x = toFloat width - 2
    y = 1

    renderLife number _ =
      Box.textured
        Textures.Score
        (toFloat (maxLives - number), y)
        (if number >= (maxLives - lives) then 11 else 12)
        (layers.bubble, 0)

  in
    Box.textured Textures.Score (x, y) 10 (layers.bubble, 0)
    :: (DigitsView.render (x, y) (score * 10))
    ++ (List.indexedMap renderLife (List.repeat maxLives True))
