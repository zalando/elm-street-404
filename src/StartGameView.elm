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
  , src = "click-to-start.png"
  }


elmStreet404Sprite : Sprite
elmStreet404Sprite =
  { size = (13, 2)
  , offset = (0, 0)
  , frames = 1
  , src = "404-elm-street.png"
  }



render : Signal.Address Action -> (Int, Int) -> Model.State -> List Sprite.Box
render address (width, height) state =
  if state == Model.Stopped then
    [ { sprite = clickToStartSprite
      , position = (toFloat width / 2 - 5, toFloat height / 2 - 1)
      , layer = layers.clickToStart
      , frame = 0
      , attributes = [onClick address Actions.Start]
      }
    , { sprite = elmStreet404Sprite
      , position = (toFloat width / 2 - 6.5, toFloat height / 4 - 1)
      , layer = layers.clickToStart
      , frame = 0
      , attributes = []
      }
    ]
  else
    []
