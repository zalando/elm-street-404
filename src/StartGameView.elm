module StartGameView (render) where

import Sprite
import Layers exposing (layers)
import Html.Events exposing (onClick)
import Actions exposing (Action)
import Model exposing (Model)


render : Signal.Address Action -> (Int, Int) -> Model.State -> List Sprite.Box
render address (width, height) state =
  if state == Model.Stopped then
    [ Sprite.box
        Sprite.ClickToStart
        (toFloat width / 2 - 5, toFloat height / 2 - 1)
        0
        (layers.clickToStart, 0)
    , Sprite.empty
        (10, 2)
        (0, 0)
        (toFloat width / 2 - 5, toFloat height / 2 - 1)
        (layers.clickToStartAbove, 0)
        [onClick address Actions.Start]
    , Sprite.box
        Sprite.ElmStreet404
        (toFloat width / 2 - 6.5, toFloat height / 4 - 1)
        0
        (layers.shadow, 0)
    ]
  else
    []
