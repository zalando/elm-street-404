module WarehouseView (render) where

import Warehouse exposing (Warehouse)
import Sprite exposing (Sprite)
import Article exposing (Article)
import Category exposing (Category)
import CategoryView
import Actions exposing (Action)
import Html.Events exposing (onClick)
import Layers exposing (layers)


warehouseSprite : Sprite
warehouseSprite =
  { size = (4, 4)
  , offset = (0, -1)
  , frames = 1
  , src = "img/warehouse.png"
  }


warehouseShadowSprite : Sprite
warehouseShadowSprite =
  { size = (5, 4)
  , offset = (0, 0)
  , frames = 1
  , src = "img/warehouse-shadow.png"
  }


warehouseBubbleSprite : Sprite
warehouseBubbleSprite =
  { size = (4, 5)
  , offset = (-2, -3)
  , frames = 1
  , src = "img/warehouse-bubble.png"
  }

emptySprite : Sprite
emptySprite = Sprite.empty (4, 4) (0, -1)

render : Signal.Address Action -> List Article -> Warehouse ->  List Sprite.Box
render address articles warehouse =
  let
    (x, y) = warehouse.position
    articlesInWarehouse = List.filter (Article.inWarehouse warehouse) articles
    numberOfArticles = List.length articlesInWarehouse
    placeholders = List.repeat (6 - numberOfArticles) Category.Placeholder
    renderArticle number article =
      CategoryView.render
        ( toFloat (number % 2) + x - 1
        , toFloat (number // 2) + y - 2
        )
        [onClick address (Actions.ClickArticle article)]
        article.category
    renderCategory number category =
      CategoryView.render
        ( toFloat ((numberOfArticles + number) % 2) + x - 1
        , toFloat ((numberOfArticles + number) // 2) + y - 2
        )
        []
        category
  in
    [ { sprite = warehouseSprite
      , position = warehouse.position
      , layer = layers.obstacle
      , frame = 0
      , attributes = []
      }
    , { sprite = warehouseShadowSprite
      , position = warehouse.position
      , layer = layers.shadow
      , frame = 0
      , attributes = []
      }
    , { sprite = warehouseBubbleSprite
      , position = warehouse.position
      , layer = layers.bubble
      , frame = 0
      , attributes = []
      }
    , { sprite = emptySprite
      , position = warehouse.position
      , layer = layers.clickAbove
      , frame = 0
      , attributes =
        [ onClick address (Actions.ClickWarehouse warehouse) ]
      }
    ] ++ List.indexedMap renderArticle (articlesInWarehouse) ++ List.indexedMap renderCategory placeholders
