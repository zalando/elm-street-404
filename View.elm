module View (view) where
import Actions exposing (Action)
import Html exposing (div, br, Html, text, button)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Model exposing (Model)
import Sprite

(=>) : a -> b -> (a, b)
(=>) = (,)


houseSprite : Sprite.Sprite
houseSprite =
  { size = (80, 120)
  , offset = (0, 0)
  , align = Sprite.Bottom
  , frames = 1
  , src = "img/house.png"
  }


houseShadowSprite : Sprite.Sprite
houseShadowSprite =
  { size = (120, 80)
  , offset = (0, 0)
  , align = Sprite.Top
  , frames = 1
  , src = "img/house-shadow.png"
  }

fountainShadowSprite : Sprite.Sprite
fountainShadowSprite =
  { size = (160, 80)
  , offset = (0, -40)
  , align = Sprite.Top
  , frames = 1
  , src = "img/fountain-shadow.png"
  }

fountainSprite : Sprite.Sprite
fountainSprite =
  { size = (120, 80)
  , offset = (0, 0)
  , align = Sprite.Bottom
  , frames = 1
  , src = "img/fountain.png"
  }

fountainSpringSprite : Sprite.Sprite
fountainSpringSprite =
  { size = (40, 80)
  , offset = (40, -40)
  , align = Sprite.Bottom
  , frames = 4
  , src = "img/fountain-spring.png"
  }



boxes : Model -> List Sprite.Box
boxes model =
  [ { sprite = houseSprite
    , position = (360, 400)
    , layer = 2
    , frame = 0
    , attributes = []
    }
  , { sprite = houseShadowSprite
    , position = (360, 360)
    , layer = 1
    , frame = 0
    , attributes = []
    }
  , { sprite = houseSprite
    , position = (280, 320)
    , layer = 2
    , frame = 0
    , attributes = []
    }
  , { sprite = houseShadowSprite
    , position = (280, 280)
    , layer = 1
    , frame = 0
    , attributes = []
    }
  , { sprite = fountainSprite
    , position = (440, 280)
    , layer = 2
    , frame = 0
    , attributes = []
    }
  , { sprite = fountainSpringSprite
    , position = (440, 280)
    , layer = 3
    , frame = Maybe.withDefault 0 (List.head model.fountain.frames)
    , attributes = []
    }
  , { sprite = fountainShadowSprite
    , position = (440, 280)
    , layer = 1
    , frame = 0
    , attributes = []
    }
  ]


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
  [ div [] (List.map Sprite.render (Sprite.sort (boxes model)))
  ]
