module WarehouseView (render) where

import MapObject exposing (MapObject)
import Sprite
import Article exposing (Article)
import Category exposing (Category)
import CategoryView
import ArticleView
import Actions exposing (Action)
import Html.Events exposing (onClick)
import Layers exposing (layers)


render : Signal.Address Action -> List Article -> Int -> MapObject -> List Sprite.Box
render address articles capacity warehouse =
  let
    (x, y) = warehouse.position
    articlesInWarehouse = List.filter (Article.inWarehouse warehouse) articles
    numberOfArticles = List.length articlesInWarehouse
    placeholders = List.repeat (capacity - numberOfArticles) Category.Placeholder

    renderArticle number =
      ArticleView.render address (toFloat (number % 2) + x - 1, toFloat (number // 2) + y - 2)

    renderCategory number =
      CategoryView.render
        ( toFloat ((numberOfArticles + number) % 2) + x - 1
        , toFloat ((numberOfArticles + number) // 2) + y - 2
        )
        Nothing

    renderBubble =
      if List.length articlesInWarehouse == 0 then
        []
      else
        ( Sprite.box
            Sprite.WarehouseBubble
            warehouse.position
            0
            (layers.bubble, 0)
        ) ::
        List.concat (List.indexedMap renderArticle articlesInWarehouse) ++
        List.concat (List.indexedMap renderCategory placeholders)
  in
    [ Sprite.box
        Sprite.Warehouse
        warehouse.position
        0
        (layers.obstacle, 0)
    , Sprite.box
        Sprite.WarehouseShadow
        warehouse.position
        0
        (layers.shadow, 0)
    , Sprite.clickable
        (4, 4)
        (0, -1)
        warehouse.position
        (layers.clickAbove, 0)
        (onClick address (Actions.ClickMapObject warehouse))
    ]
    ++ renderBubble
