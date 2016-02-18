module ScoreView (render) where

import Sprite exposing (Sprite)
import Layers exposing (layers)
import DigitsView


scoreSprite : Sprite
scoreSprite =
  { size = (1, 1)
  , offset = (0, 0)
  , frames = 13
  , src = "score.png"
  }


render : (Int, Int) -> Int -> Int -> Int -> List Sprite.Box
render (width, height) score maxLives lives =
  let
    x = toFloat width - 2
    y = 1

    renderLife number _ =
      { sprite = scoreSprite
      , position = (toFloat (maxLives - number), y)
      , layer = layers.customer
      , frame = if number >= (maxLives - lives) then 11 else 12
      , attributes = []
      }

  in
    { sprite = scoreSprite
    , position = (x, y)
    , layer = layers.customer
    , frame = 10
    , attributes = []
    }
    :: (DigitsView.render (x, y) (score * 10))
    ++ (List.indexedMap renderLife (List.repeat maxLives True))
