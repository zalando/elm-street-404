module WarehouseView (render) where

import Warehouse exposing (Warehouse)
import Sprite exposing (Sprite)
import Article exposing (Article)
import Category exposing (Category)
import CategoryView
import ArticleView
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


render : Signal.Address Action -> List Article -> Warehouse ->  List Sprite.Box
render address articles warehouse =
  let
    (x, y) = warehouse.position
    articlesInWarehouse = List.filter (Article.inWarehouse warehouse) articles
    numberOfArticles = List.length articlesInWarehouse
    placeholders = List.repeat (warehouse.capacity - numberOfArticles) Category.Placeholder

    renderArticle number =
      ArticleView.render address (toFloat (number % 2) + x - 1, toFloat (number // 2) + y - 2)

    renderCategory number =
      CategoryView.render
        ( toFloat ((numberOfArticles + number) % 2) + x - 1
        , toFloat ((numberOfArticles + number) // 2) + y - 2
        )
        []
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
    , { sprite = Sprite.empty (4, 4) (0, -1)
      , position = warehouse.position
      , layer = layers.clickAbove
      , frame = 0
      , attributes =
        [ onClick address (Actions.ClickWarehouse warehouse) ]
      }
    ]
    ++ List.concat(List.indexedMap renderArticle articlesInWarehouse)
    ++ List.concat(List.indexedMap renderCategory placeholders)
