module StartGameView (render) where

import Sprite exposing (Sprite)
import Layers exposing (layers)
import Html.Events exposing (onClick)
import Actions exposing (Action)
import Model exposing (Model)


clickToStartSprite : Sprite
clickToStartSprite =
  { size = (10, 2)
  , offset = (0, 0)
  , frames = 1
  , src = "img/click-to-start.png"
  }


render : Signal.Address Action -> Model.State -> List Sprite.Box
render address state =
  if state == Model.Stopped then
    [ { sprite = clickToStartSprite
      , position = (6, 6)
      , layer = layers.clickToStart
      , frame = 0
      , attributes = [onClick address Actions.Start]
      }
    ]
  else
    []
