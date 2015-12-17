module HouseView (render) where

import Actions exposing (Action)
import Html.Events exposing (onClick)
import Layers exposing (layers)
import Sprite exposing (Sprite)
import House exposing (House)
import Request exposing (Request)
import CategoryView
import Category

sprite : Sprite
sprite =
  { size = (2, 3)
  , offset = (0, -1)
  , frames = 1
  , src = "img/house.png"
  }


shadowSprite : Sprite
shadowSprite =
  { size = (3, 2)
  , offset = (0, 1)
  , frames = 1
  , src = "img/house-shadow.png"
  }


bubbleSprite1 : Sprite
bubbleSprite1 =
  { size = (3, 3)
  , offset = (-2, -1)
  , frames = 1
  , src = "img/house-bubble-1.png"
  }


bubbleSprite2 : Sprite
bubbleSprite2 =
  { size = (3, 4)
  , offset = (-2, -2)
  , frames = 1
  , src = "img/house-bubble-2.png"
  }


bubbleSprite3 : Sprite
bubbleSprite3 =
  { size = (3, 5)
  , offset = (-2, -3)
  , frames = 1
  , src = "img/house-bubble-3.png"
  }


getBubbleSprite : Int -> Maybe Sprite
getBubbleSprite number =
  case number of
    0 -> Nothing
    1 -> Just bubbleSprite1
    2 -> Just bubbleSprite2
    _ -> Just bubbleSprite3


render : Signal.Address Action -> List Request -> House -> List Sprite.Box
render address requests house =
  let
    requestsFromHouse = List.filter (Request.inHouse house) requests
    renderRequest number request =
      let
        position = (fst house.position - 1, snd house.position - toFloat number)
      in
      case request of
        Request.Return _ article _ ->
          [ CategoryView.render position [] (Request.category request)
          , CategoryView.render
              position
              [onClick address (Actions.ClickArticle article)]
              Category.Return
          ]
        Request.Order house category _ ->
          [ CategoryView.render
              position
              [onClick address (Actions.ClickCategory category)]
              (Request.category request)
          ]
    renderBubble =
      case getBubbleSprite (List.length requestsFromHouse) of
        Just sprite ->
          [ { sprite = sprite
            , position = house.position
            , layer = layers.bubble
            , frame = 0
            , attributes = []
            }
          ]
        _ -> []
  in
    [ { sprite = sprite
      , position = house.position
      , layer = layers.obstacle
      , frame = 0
      , attributes =
        [ onClick address (Actions.ClickHouse house)]
      }
    , { sprite = shadowSprite
      , position = house.position
      , layer = layers.shadow
      , frame = 0
      , attributes = []
      }
    ] ++ List.concat (List.indexedMap renderRequest requestsFromHouse) ++ renderBubble
