module View (view) where
import Actions exposing (Action)
import Html exposing (div, br, Html, text, button)
import Html.Attributes exposing (style)
import Model exposing (Model)
import Sprite
import Obstacle

(=>) : a -> b -> (a, b)
(=>) = (,)


houseSprite : Sprite.Sprite
houseSprite =
  { size = (2, 3)
  , offset = (0, -1)
  , frames = 1
  , src = "img/house.png"
  }


houseShadowSprite : Sprite.Sprite
houseShadowSprite =
  { size = (3, 2)
  , offset = (0, 1)
  , frames = 1
  , src = "img/house-shadow.png"
  }



boxes : Model -> List Sprite.Box
boxes model =
  List.concat (
    [ { sprite = houseSprite
      , position = (8, 10)
      , layer = 2
      , frame = 0
      , attributes = []
      }
    , { sprite = houseShadowSprite
      , position = (8, 10)
      , layer = 1
      , frame = 0
      , attributes = []
      }
    , { sprite = houseSprite
      , position = (7, 7)
      , layer = 2
      , frame = 0
      , attributes = []
      }
    , { sprite = houseShadowSprite
      , position = (7, 7)
      , layer = 1
      , frame = 0
      , attributes = []
      }
    ] :: (List.map Obstacle.render model.obstacles)
  )

view : Signal.Address Action -> Model -> Html
view address model =
  div
  [ style
    [ "height" => "560px"
    , "margin" => "auto"
    , "position" => "relative"
    , "width" => "960px"
    , "background-image" => "url(img/bg-grid.jpg)"
    , "background-size" => "960px 560px"
    ]
  ]
  [ div [] (List.map (Sprite.render model.tileSize) (Sprite.sort (boxes model)))
  ]
