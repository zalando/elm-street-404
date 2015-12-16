module WarehouseView (render) where

import Warehouse exposing (Warehouse, warehouseSprite, warehouseShadowSprite, warehouseBubbleSprite)
import Sprite exposing (Sprite)
import Article exposing (Article)
import Category exposing (Category)
import Actions exposing (Action)
import Html.Events exposing (onClick)
import Layers exposing (layers)


render : Signal.Address Action -> List Article -> Warehouse ->  List Sprite.Box
render address articles warehouse =
  let
    warehouseCoordinates = warehouse.position
    articlesInWarehouse = List.filter (Article.inWarehouse warehouse) articles
    placeholders = List.repeat (6 - List.length articlesInWarehouse) Category.Placeholder
    renderCategory number category =
      Category.render (toFloat (number % 2) + fst warehouseCoordinates - 1, toFloat (number // 2) + snd warehouseCoordinates - 2) category
  in
    [ { sprite = warehouseSprite
      , position = warehouse.position
      , layer = layers.obstacle
      , frame = 0
      , attributes =
        [ onClick address (Actions.GoTo (round (fst warehouse.position) + 1, round (snd warehouse.position + snd warehouse.size))) ]
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
    ] ++ List.indexedMap renderCategory (List.map .category articlesInWarehouse ++ placeholders)
